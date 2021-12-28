//! Compression Filters
//!
//! Filters that compress the body of a response.

#[cfg(feature = "compression-brotli")]
use async_compression::{tokio::bufread::BrotliEncoder, Level as CompressionLevel};

#[cfg(feature = "compression-gzip")]
use async_compression::tokio::bufread::{DeflateEncoder, GzipEncoder};

use headers::ContentCoding;
use headers::Header;
use http::header::HeaderValue;
use hyper::{
    header::{CONTENT_ENCODING, CONTENT_LENGTH},
    Body,
};
use std::convert::TryFrom;
use tokio_util::io::{ReaderStream, StreamReader};

use crate::filter::{Filter, WrapSealed};
use crate::reject::IsReject;
use crate::reply::{Reply, Response};

use self::internal::{CompressionProps, WithCompression};

enum CompressionAlgo {
    #[cfg(feature = "compression-brotli")]
    BR,
    #[cfg(feature = "compression-gzip")]
    DEFLATE,
    #[cfg(feature = "compression-gzip")]
    GZIP,
}

impl From<CompressionAlgo> for HeaderValue {
    #[inline]
    fn from(algo: CompressionAlgo) -> Self {
        HeaderValue::from_static(match algo {
            #[cfg(feature = "compression-brotli")]
            CompressionAlgo::BR => "br",
            #[cfg(feature = "compression-gzip")]
            CompressionAlgo::DEFLATE => "deflate",
            #[cfg(feature = "compression-gzip")]
            CompressionAlgo::GZIP => "gzip",
        })
    }
}

impl CompressionAlgo {
    #[inline]
    fn to_str(&self) -> &'static str {
        match self {
            CompressionAlgo::BR => "br",
            CompressionAlgo::DEFLATE => "deflate",
            CompressionAlgo::GZIP => "gzip",
        }
    }
}

/// Compression
#[derive(Clone, Copy, Debug)]
pub struct Compression<F> {
    func: F,
}

// TODO: The implementation of `gzip()`, `deflate()`, and `brotli()` could be replaced with
// generics or a macro

/// Create a wrapping filter that compresses the Body of a [`Response`](crate::reply::Response)
/// using whatever value is specified in the `Accept-Encoding` header, adding
/// `content-encoding: <coding>` to the Response's [`HeaderMap`](hyper::HeaderMap)
///
/// # Example
///
/// ```
/// use warp::Filter;
///
/// let route = warp::get()
///     .and(warp::path::end())
///     .and(warp::fs::file("./README.md"))
///     .with(warp::compression::auto());
/// ```
pub fn auto() -> Compression<impl Fn(CompressionProps) -> Response + Copy> {
    let func = move |props: CompressionProps| {
        if let Some(ref header) = props.accept_enc {
            if let Some(prefered_encoding) = header.prefered_encoding() {
                let enc = headers::ContentEncoding::decode(
                    &mut props
                        .head
                        .headers
                        .get_all(http::header::CONTENT_ENCODING)
                        .iter(),
                );
                let alreayd_encoded = if let Ok(ref encoding) = enc {
                    encoding.contains(prefered_encoding.to_static())
                } else {
                    false
                };
                return if alreayd_encoded {
                    Response::from_parts(props.head, Body::wrap_stream(props.body))
                } else {
                    match prefered_encoding {
                        ContentCoding::GZIP => (gzip().func)(props),
                        ContentCoding::DEFLATE => (deflate().func)(props),
                        ContentCoding::BROTLI => (brotli().func)(props),
                        _ => Response::from_parts(props.head, Body::wrap_stream(props.body)),
                    }
                };
            }
        }
        Response::from_parts(props.head, Body::wrap_stream(props.body))
    };

    Compression { func }
}

/// Given an optional existing encoding header, appends to the existing or creates a new one
fn create_encoding_header(existing: Option<HeaderValue>, coding: ContentCoding) -> HeaderValue {
    if let Some(val) = existing {
        if let Ok(str_val) = val.to_str() {
            return HeaderValue::try_from(&format!("{}, {}", str_val, coding.to_string()))
                .unwrap_or_else(|_| coding.into());
        }
    }
    coding.into()
}

/// Create a wrapping filter that compresses the Body of a [`Response`](crate::reply::Response)
/// using gzip, adding `content-encoding: gzip` to the Response's [`HeaderMap`](hyper::HeaderMap)
///
/// # Example
///
/// ```
/// use warp::Filter;
///
/// let route = warp::get()
///     .and(warp::path::end())
///     .and(warp::fs::file("./README.md"))
///     .with(warp::compression::gzip());
/// ```
#[cfg(feature = "compression-gzip")]
pub fn gzip() -> Compression<impl Fn(CompressionProps) -> Response + Copy> {
    let func = move |mut props: CompressionProps| {
        let body = Body::wrap_stream(ReaderStream::new(GzipEncoder::new(StreamReader::new(
            props.body,
        ))));
        let header = create_encoding_header(
            props.head.headers.remove(CONTENT_ENCODING),
            ContentCoding::GZIP,
        );
        props.head.headers.append(CONTENT_ENCODING, header);
        props.head.headers.remove(CONTENT_LENGTH);
        Response::from_parts(props.head, body)
    };
    Compression { func }
}

/// Create a wrapping filter that compresses the Body of a [`Response`](crate::reply::Response)
/// using deflate, adding `content-encoding: deflate` to the Response's [`HeaderMap`](hyper::HeaderMap)
///
/// # Example
///
/// ```
/// use warp::Filter;
///
/// let route = warp::get()
///     .and(warp::path::end())
///     .and(warp::fs::file("./README.md"))
///     .with(warp::compression::deflate());
/// ```
#[cfg(feature = "compression-gzip")]
pub fn deflate() -> Compression<impl Fn(CompressionProps) -> Response + Copy> {
    let func = move |mut props: CompressionProps| {
        let body = Body::wrap_stream(ReaderStream::new(DeflateEncoder::new(StreamReader::new(
            props.body,
        ))));
        let header = create_encoding_header(
            props.head.headers.remove(CONTENT_ENCODING),
            ContentCoding::DEFLATE,
        );
        props.head.headers.append(CONTENT_ENCODING, header);
        props.head.headers.remove(CONTENT_LENGTH);
        Response::from_parts(props.head, body)
    };
    Compression { func }
}

/// Create a wrapping filter that compresses the Body of a [`Response`](crate::reply::Response)
/// using brotli, adding `content-encoding: br` to the Response's [`HeaderMap`](hyper::HeaderMap)
///
/// # Example
///
/// ```
/// use warp::Filter;
///
/// let route = warp::get()
///     .and(warp::path::end())
///     .and(warp::fs::file("./README.md"))
///     .with(warp::compression::brotli());
/// ```
#[cfg(feature = "compression-brotli")]
pub fn brotli() -> Compression<impl Fn(CompressionProps) -> Response + Copy> {
    let func = move |mut props: CompressionProps| {
        let body = Body::wrap_stream(ReaderStream::new(BrotliEncoder::with_quality(
            StreamReader::new(props.body),
            CompressionLevel::Precise(4),
        )));
        let header = create_encoding_header(
            props.head.headers.remove(CONTENT_ENCODING),
            ContentCoding::BROTLI,
        );
        props.head.headers.append(CONTENT_ENCODING, header);
        props.head.headers.remove(CONTENT_LENGTH);
        Response::from_parts(props.head, body)
    };
    Compression { func }
}

impl<FN, F> WrapSealed<F> for Compression<FN>
where
    FN: Fn(CompressionProps) -> Response + Clone + Send,
    F: Filter + Clone + Send,
    F::Extract: Reply,
    F::Error: IsReject,
{
    type Wrapped = WithCompression<FN, F>;

    fn wrap(&self, filter: F) -> Self::Wrapped {
        WithCompression {
            filter,
            compress: self.clone(),
        }
    }
}

mod internal {
    use std::future::Future;
    use std::pin::Pin;
    use std::task::{Context, Poll};

    use bytes::Bytes;
    use futures_util::{ready, Stream, TryFuture};
    use headers::HeaderMapExt;
    use hyper::Body;
    use pin_project::pin_project;

    use crate::filter::{Filter, FilterBase, Internal};
    use crate::reject::IsReject;
    use crate::reply::{Reply, Response};
    use crate::route;

    use super::Compression;

    /// A wrapper around any type that implements [`Stream`](futures::Stream) to be
    /// compatible with async_compression's Stream based encoders
    #[pin_project]
    #[derive(Debug)]
    pub struct CompressableBody<S, E>
    where
        E: std::error::Error,
        S: Stream<Item = Result<Bytes, E>>,
    {
        #[pin]
        body: S,
    }

    impl<S, E> Stream for CompressableBody<S, E>
    where
        E: std::error::Error,
        S: Stream<Item = Result<Bytes, E>>,
    {
        type Item = std::io::Result<Bytes>;

        fn poll_next(self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Option<Self::Item>> {
            use std::io::{Error, ErrorKind};

            let pin = self.project();
            S::poll_next(pin.body, cx).map_err(|_| Error::from(ErrorKind::InvalidData))
        }
    }

    impl From<Body> for CompressableBody<Body, hyper::Error> {
        fn from(body: Body) -> Self {
            CompressableBody { body }
        }
    }

    /// Compression Props
    #[derive(Debug)]
    pub struct CompressionProps {
        pub(super) body: CompressableBody<Body, hyper::Error>,
        pub(super) head: http::response::Parts,
        pub(super) accept_enc: Option<headers::AcceptEncoding>,
    }

    #[allow(missing_debug_implementations)]
    pub struct Compressed(pub(super) Response);

    impl Reply for Compressed {
        #[inline]
        fn into_response(self) -> Response {
            self.0
        }
    }

    #[allow(missing_debug_implementations)]
    #[derive(Clone, Copy)]
    pub struct WithCompression<FN, F> {
        pub(super) compress: Compression<FN>,
        pub(super) filter: F,
    }

    impl<FN, F> FilterBase for WithCompression<FN, F>
    where
        FN: Fn(CompressionProps) -> Response + Clone + Send,
        F: Filter + Clone + Send,
        F::Extract: Reply,
        F::Error: IsReject,
    {
        type Extract = (Compressed,);
        type Error = F::Error;
        type Future = WithCompressionFuture<FN, F::Future>;

        fn filter(&self, _: Internal) -> Self::Future {
            WithCompressionFuture {
                compress: self.compress.clone(),
                future: self.filter.filter(Internal),
            }
        }
    }

    #[allow(missing_debug_implementations)]
    #[pin_project]
    pub struct WithCompressionFuture<FN, F> {
        compress: Compression<FN>,
        #[pin]
        future: F,
    }

    impl<FN, F> Future for WithCompressionFuture<FN, F>
    where
        FN: Fn(CompressionProps) -> Response,
        F: TryFuture,
        F::Ok: Reply,
        F::Error: IsReject,
    {
        type Output = Result<(Compressed,), F::Error>;

        fn poll(mut self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Self::Output> {
            let pin = self.as_mut().project();
            let result = ready!(pin.future.try_poll(cx));
            match result {
                Ok(reply) => {
                    let resp = route::with(|route| {
                        let acc_enc: Option<headers::AcceptEncoding> = route.headers().typed_get();
                        let (head, body) = reply.into_response().into_parts();
                        let compress_props = CompressionProps {
                            body: body.into(),
                            head: head,
                            accept_enc: acc_enc,
                        };
                        (self.compress.func)(compress_props)
                    });
                    Poll::Ready(Ok((Compressed(resp),)))
                }
                Err(reject) => Poll::Ready(Err(reject)),
            }
        }
    }
}
