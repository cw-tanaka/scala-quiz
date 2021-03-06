package com.chatwork.quiz.misc

/**
 * ワードをカウントするオブジェクト。
 */
object WordCounter {

  /**
   * 文字列から単語数をカウントする。
   *
   * @param words 文字列
   * @return 単語がキー、単語数がヴァリューのマップ
   */
  def countWords(words: List[String]): Map[String, Int] = {
    words.flatMap(w => w.split(" ")).groupBy(identity).map {
      case (word, list) => (word, list.length)
    }
    // こう書けなくてはまった↓
    // words.flatMap(w => w.split(" ")).groupBy(identity).map((word, list) => (word, list.length))
  }
}
