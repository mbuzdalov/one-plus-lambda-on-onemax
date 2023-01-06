package com.github.mbuzdalov.oll

import scala.annotation.tailrec

/**
 * This cache tries to save roughly a given byte volume of cached entries in memory.
 * When some entries need to be discarded, those with larger goodBitsInDifference are retained, because they are
 * harder to recompute from scratch.
 *
 * @param maxCacheByteSize the (approximate) maximum size of all the cache entries, in bytes.
 * @param delegate the crossover computation class used to perform the math.
 */
class InMemoryCostPrioritizingCrossoverCache(maxCacheByteSize: Long,
                                             delegate: CrossoverComputation,
                                             verbose: Boolean) extends CrossoverComputation {
  import InMemoryCostPrioritizingCrossoverCache.CacheEntry

  private val entryOrdering: Ordering[CacheEntry] = (x: CacheEntry, y: CacheEntry) => -java.lang.Long.compare(x.g, y.g)
  private val cache = new scala.collection.mutable.HashMap[CacheEntry, CacheEntry]
  private val queue = new scala.collection.mutable.PriorityQueue[CacheEntry]()(entryOrdering)
  private var byteSize, queries, hits, hitTime, totalHits, misses, missTime, totalMisses = 0L

  @tailrec
  private def tryAddEntry(e: CacheEntry): Unit = {
    val newCacheByteSize = byteSize + e.byteSize
    if (newCacheByteSize <= maxCacheByteSize) {
      cache.put(e, e)
      queue.addOne(e)
      byteSize = newCacheByteSize
    } else {
      val queueTop = queue.head
      if (e.g > queueTop.g) {
        queue.dequeue()
        cache.remove(queueTop)
        byteSize -= queueTop.byteSize
        tryAddEntry(e)
      }
    }
  }

  override def compute(distanceToParent: Int, goodBitsInDifference: Int, populationSize: Int, crossoverBias: Double): Array[Double] = {
    val entry = CacheEntry(distanceToParent, goodBitsInDifference, populationSize, crossoverBias)
    cache.synchronized {
      assert(cache.size == queue.size)

      val timeCost = goodBitsInDifference.toLong * goodBitsInDifference
      val realEntry = if (cache.contains(entry)) {
        hits += 1
        hitTime += timeCost
        cache(entry)
      } else {
        misses += 1
        missTime += timeCost
        tryAddEntry(entry)
        entry
      }
      queries += 1
      if (queries % 1000000 == 0) {
        totalHits += hits
        totalMisses += misses
        if (verbose) {
          println(s"[$queries queries, $totalHits hits ($hits new, cost $hitTime), $totalMisses misses ($misses new, cost $missTime), cache size ${cache.size}, $byteSize bytes in arrays]")
        }
        hits = 0
        misses = 0
        hitTime = 0
        missTime = 0
      }
      realEntry
    }.result(delegate)
  }

  override def clear(): Unit = {
    cache.clear()
    queue.clear()
    byteSize = 0
    queries = 0
    hits = 0
    hitTime = 0
    totalHits = 0
    misses = 0
    missTime = 0
    totalMisses = 0
  }
}

object InMemoryCostPrioritizingCrossoverCache {
  private case class CacheEntry(d: Int, g: Int, popSize: Int, xProb: Double) {
    private[this] var cachedResult: Array[Double] = _

    def byteSize: Int = (g + 5) * 8

    def result(delegate: CrossoverComputation): Array[Double] = synchronized {
      if (cachedResult == null) {
        cachedResult = delegate.compute(d, g, popSize, xProb)
      }
      cachedResult
    }
  }
}
