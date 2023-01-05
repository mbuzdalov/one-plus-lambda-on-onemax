package com.github.mbuzdalov.oll

import scala.annotation.tailrec

/**
 * This cache tries to save roughly a given byte volume of cached entries in memory.
 * When some entries need to be discarded, those with larger goodBitsInDifference are retained, because they are
 * harder to recompute from scratch.
 *
 * @param delegate the crossover computation class used to perform the math.
 */
class InMemoryCostPrioritizingCrossoverCache(maxCacheByteSize: Long, delegate: CrossoverComputation) extends CrossoverComputation {
  import InMemoryCostPrioritizingCrossoverCache.CacheEntry

  private val cacheEntryOrdering: Ordering[CacheEntry] =
    (x: CacheEntry, y: CacheEntry) => -java.lang.Long.compare(x.g, y.g)
  private val probOfReachingFCache = new scala.collection.mutable.HashMap[CacheEntry, CacheEntry]
  private val cacheEntryQueue = new scala.collection.mutable.PriorityQueue[CacheEntry]()(cacheEntryOrdering)
  private var cacheByteSize, queries, hits, hitTime, totalHits, misses, missTime, totalMisses = 0L

  @tailrec
  private def tryAddEntry(e: CacheEntry): Unit = {
    val newCacheByteSize = cacheByteSize + e.byteSize
    if (newCacheByteSize <= maxCacheByteSize) {
      probOfReachingFCache.put(e, e)
      cacheEntryQueue.addOne(e)
      cacheByteSize = newCacheByteSize
    } else {
      val queueTop = cacheEntryQueue.head
      if (e.g > queueTop.g) {
        cacheEntryQueue.dequeue()
        probOfReachingFCache.remove(queueTop)
        cacheByteSize -= queueTop.byteSize
        tryAddEntry(e)
      }
    }
  }

  override def compute(distanceToParent: Int, goodBitsInDifference: Int, populationSize: Int, crossoverBias: Double): Array[Double] = {
    val entry = CacheEntry(distanceToParent, goodBitsInDifference, populationSize, crossoverBias)
    probOfReachingFCache.synchronized {
      assert(probOfReachingFCache.size == cacheEntryQueue.size)

      val timeCost = goodBitsInDifference.toLong * goodBitsInDifference
      val realEntry = if (probOfReachingFCache.contains(entry)) {
        hits += 1
        hitTime += timeCost
        probOfReachingFCache(entry)
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
        println(s"[$queries queries, $totalHits hits ($hits new, cost $hitTime), $totalMisses misses ($misses new, cost $missTime), cache size ${probOfReachingFCache.size}, $cacheByteSize bytes in arrays]")
        hits = 0
        misses = 0
        hitTime = 0
        missTime = 0
      }
      realEntry
    }.result(delegate)
  }

  override def clear(): Unit = {
    probOfReachingFCache.clear()
  }
}

object InMemoryCostPrioritizingCrossoverCache {
  private case class CacheEntry(d: Int, g: Int, popSize: Int, xProb: Double) {
    private[this] var cachedResult: Array[Double] = _

    def byteSize: Int = (g + 5) * 8

    def result(delegate: CrossoverComputation): Array[Double] = {
      if (cachedResult == null) {
        cachedResult = delegate.compute(d, g, popSize, xProb)
      }
      cachedResult
    }
  }
}
