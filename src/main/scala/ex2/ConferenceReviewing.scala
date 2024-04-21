package ex2

import java.util
import Review.{*, given}
import ex2.ConferenceReviewing.Question

import scala.jdk.CollectionConverters.*

class ConferenceReviewingJavaFacade extends ConferenceReviewing:

  private var data: Map[Int, Seq[Review]] = Map()

  override def loadReview(article: Int, scores: util.Map[ConferenceReviewing.Question, Integer]): Unit =
    if ! data.contains(article) then
      data = data + ((article, List()))

    val previousReviews = data(article)
    data = data + ((article, previousReviews :+ scores))

  override def loadReview(article: Int, relevance: Int, significance: Int, confidence: Int, fin: Int): Unit =
    loadReview(article, Review(relevance, significance, confidence, fin))

  override def orderedScores(article: Int, question: ConferenceReviewing.Question): util.List[Integer] =
    data(article).map(_.getScoreFor(question)).sorted.asJava

  override def averageFinalScore(article: Int): Double =
    val scores = data(article).map(_.getScoreFor(Question.FINAL))
    scores.sum.toDouble / scores.length.toDouble

  private def acceptedArticlesMap =
    data.filter((id, reviews) =>
      val averageScore = averageFinalScore(id)
      val relGteEight = reviews.count(_.getScoreFor(Question.RELEVANCE) >= 8)
      averageScore > 5 && relGteEight >= 1
    )

  override def acceptedArticles: util.Set[Integer] =
    acceptedArticlesMap.map(_._1).toSet.asJava

  override def sortedAcceptedArticles(): java.util.List[ex2.Pair[Integer, java.lang.Double]] =
    acceptedArticlesMap.map((k, v) => (k, averageFinalScore(k))).toList.asJava

  override def averageWeightedFinalScoreMap(): java.util.Map[java.lang.Integer, java.lang.Double] =
    data.map((k, v) => (k, v.map(_.weightedFinalScore).sum / v.length)).asJava
