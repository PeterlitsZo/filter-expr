//! Target-specific caches for compiled expressions and regular expressions.

use std::{borrow::Borrow, hash::Hash, sync::Arc};

use crate::Result;

#[cfg(not(all(target_arch = "wasm32", target_os = "unknown")))]
pub(crate) struct Cache<K, V>(moka::sync::Cache<K, Arc<V>>);

#[cfg(all(target_arch = "wasm32", target_os = "unknown"))]
type WasmCache<K, V> = lru::LruCache<
    K,
    Arc<V>,
    std::hash::BuildHasherDefault<std::collections::hash_map::DefaultHasher>,
>;

#[cfg(all(target_arch = "wasm32", target_os = "unknown"))]
pub(crate) struct Cache<K, V>(std::sync::Mutex<WasmCache<K, V>>);

impl<K, V> Cache<K, V>
where
    K: Hash + Eq + Send + Sync + 'static,
    V: Send + Sync + 'static,
{
    pub(crate) fn new(capacity: usize) -> Self {
        #[cfg(not(all(target_arch = "wasm32", target_os = "unknown")))]
        {
            Self(moka::sync::Cache::new(capacity as u64))
        }
        #[cfg(all(target_arch = "wasm32", target_os = "unknown"))]
        {
            // Explicit deterministic hashing avoids requiring a host random source.
            Self(std::sync::Mutex::new(lru::LruCache::with_hasher(
                std::num::NonZeroUsize::new(capacity).expect("cache capacity must be positive"),
                Default::default(),
            )))
        }
    }

    pub(crate) fn get<Q>(&self, key: &Q) -> Result<Option<Arc<V>>>
    where
        K: Borrow<Q>,
        Q: Hash + Eq + ?Sized,
    {
        #[cfg(not(all(target_arch = "wasm32", target_os = "unknown")))]
        {
            Ok(self.0.get(key))
        }
        #[cfg(all(target_arch = "wasm32", target_os = "unknown"))]
        {
            Ok(self.lock()?.get(key).cloned())
        }
    }

    pub(crate) fn insert(&self, key: K, value: Arc<V>) -> Result<()> {
        #[cfg(not(all(target_arch = "wasm32", target_os = "unknown")))]
        self.0.insert(key, value);
        #[cfg(all(target_arch = "wasm32", target_os = "unknown"))]
        self.lock()?.put(key, value);
        Ok(())
    }

    #[cfg(all(target_arch = "wasm32", target_os = "unknown"))]
    fn lock(&self) -> Result<std::sync::MutexGuard<'_, WasmCache<K, V>>> {
        self.0.lock().map_err(|e| {
            crate::Error::new(crate::ErrorKind::Internal, "failed to lock cache")
                .with_metadata("error", e.to_string())
        })
    }
}
