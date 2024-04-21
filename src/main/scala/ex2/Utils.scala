package ex2

import ConferenceReviewing.Question
import scala.jdk.CollectionConverters._

case class Pair[A, B](left: A, right: B):
  given Conversion[(A, B), Pair[A, B]] with
    override def apply(x: (A, B)): Pair[A, B] = Pair(x._1, x._2)

case class Review(relevance: Int, significance: Int, confidence: Int, fin: Int):
  def getScoreFor(question: Question) =
    question match
      case Question.FINAL         => this.fin
      case Question.RELEVANCE     => this.relevance
      case Question.CONFIDENCE    => this.confidence
      case Question.SIGNIFICANCE  => this.significance

  def weightedFinalScore = (confidence * fin) / 10.0

  given Conversion[java.util.Map[Question, java.lang.Integer], Review] with
    override def apply(x: java.util.Map[Question, Int]): Review =
      Review(
        x.get(Question.RELEVANCE),
        x.get(Question.SIGNIFICANCE),
        x.get(Question.CONFIDENCE),
        x.get(Question.FINAL),
      )

  given Conversion[Review, java.util.Map[Question, java.lang.Integer]] with
    override def apply(x: Review): java.util.Map[Question, Int] =
      Map(
        Question.RELEVANCE -> x.relevance,
        Question.SIGNIFICANCE -> x.significance,
        Question.CONFIDENCE -> x.confidence,
        Question.FINAL -> x.fin
      ).asJava