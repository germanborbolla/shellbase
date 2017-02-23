/**
 * Licensed to the Apache Software Foundation (ASF) under one
 * or more contributor license agreements.  See the NOTICE file
 * distributed with this work for additional information
 * regarding copyright ownership.  The ASF licenses this file
 * to you under the Apache License, Version 2.0 (the
 * "License"); you may not use this file except in compliance
 * with the License.  You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing,
 * software distributed under the License is distributed on an
 * "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
 * KIND, either express or implied.  See the License for the
 * specific language governing permissions and limitations
 * under the License.
 */
package com.sumologic.shellbase

import java.io.InputStream
import java.text.ParseException
import java.util.Date

import jline.console.ConsoleReader
import org.junit.runner.RunWith
import org.mockito.Matchers._
import org.mockito.Mockito._
import org.mockito.invocation.InvocationOnMock
import org.mockito.stubbing.Answer
import org.scalatest.BeforeAndAfterEach
import org.scalatest.junit.JUnitRunner
import org.scalatest.mock.MockitoSugar

import scala.collection.mutable

@RunWith(classOf[JUnitRunner])
class ShellPrompterTest extends CommonWordSpec with BeforeAndAfterEach with MockitoSugar {
  "ShellPrompter.confirm" should {
    "return true for yes answers" in {
      feedCharacters("y")
      sut.confirm("blah") should be(true)

      feedCharacters("Y")
      sut.confirm("blah") should be(true)
    }

    "return false for no answers" in {
      feedCharacters("n")
      sut.confirm("blah") should be(false)

      feedCharacters("N")
      sut.confirm("blah") should be(false)
    }

    "handle bogus input until valid character" in {
      feedCharacters("fasdfkljn")
      sut.confirm("blah") should be(false)
    }
  }

  "ShellPrompter.confirmWithDefault" should {
    "return true for yes answers" in {
      feedCharacters("y")
      sut.confirmWithDefault("blah", default = false) should be(true)

      feedCharacters("Y")
      sut.confirmWithDefault("blah", default = false) should be(true)
    }

    "return false for no answers" in {
      feedCharacters("n")
      sut.confirmWithDefault("blah", default = true) should be(false)

      feedCharacters("N")
      sut.confirmWithDefault("blah", default = true) should be(false)
    }

    "return the default for line feed" in {
      feedCharacters(sut.asciiCR)
      sut.confirmWithDefault("blah", default = false) should be(false)

      feedCharacters(sut.asciiCR)
      sut.confirmWithDefault("blah", default = true) should be(true)
    }
  }

  "ShellPrompter.readChar" should {
    "accept any character if not specified" in {
      feedCharacters("abc")
      sut.readChar("blah", Seq.empty) should be('a'.toInt)
      sut.readChar("blah", Seq.empty) should be('b'.toInt)
      sut.readChar("blah", Seq.empty) should be('c'.toInt)
    }

    "accept a subset of characters if specified" in {
      feedCharacters("abc")
      sut.readChar("blah", Seq('a', 'c')) should be('a'.toInt)
      sut.readChar("blah", Seq('a', 'c')) should be('c'.toInt)
    }
  }

  "ShellPrompter.confirmWithWarning" should {
    "return true for yes answers" in {
      feedCharacters("y")
      sut.confirmWithWarning("blah") should be(true)

      feedCharacters("Y")
      sut.confirmWithWarning("blah") should be(true)
    }

    "return false for no answers" in {
      feedCharacters("n")
      sut.confirmWithWarning("blah") should be(false)

      feedCharacters("N")
      sut.confirmWithWarning("blah") should be(false)
    }

    "handle bogus input until valid character" in {
      feedCharacters("fasdfkljn")
      sut.confirmWithWarning("blah") should be(false)
    }
  }

  "ShellPrompter.askForTime" should {
    "produce date for format" in {
      when(shellIO.readLine(anyString)).thenReturn("15 Jul 22:01")

      val expectedDate = new Date()
      expectedDate.setHours(22)
      expectedDate.setMinutes(1)
      expectedDate.setMonth(6)
      expectedDate.setDate(15)
      expectedDate.setSeconds(0)
      expectedDate.setTime((expectedDate.getTime / 1000) * 1000)

      sut.askForTime("someQuestion") should be(expectedDate)
    }

    "allow custom formats + don't change year unless needed" in {
      when(shellIO.readLine(anyString)).thenReturn("15 Jul 2013 22:01")

      val expectedDate = new Date()
      expectedDate.setHours(22)
      expectedDate.setMinutes(1)
      expectedDate.setMonth(6)
      expectedDate.setDate(15)
      expectedDate.setSeconds(0)
      expectedDate.setYear(2013 - 1900)
      expectedDate.setTime((expectedDate.getTime / 1000) * 1000)

      sut.askForTime("someQuestion", "d MMM y HH:mm") should be(expectedDate)
    }

    "bubble exception for incorrect format" in {
      when(shellIO.readLine(anyString())).thenReturn("kajsdfl;kj1")
      intercept[ParseException] {
        sut.askForTime("my question")
      }
    }
  }

  "ShellPrompter.askPasswordWithConfirmation" should {
    "require a minimum length" in {
      answerQuestionWith("a", "b", "cc", "cc")
      sut.askPasswordWithConfirmation(minLength = 2) should be("cc")
    }

    "return the password if they eventually match (after retries)" in {
      answerQuestionWith("a", "b", "c", "d", "e", "e")
      sut.askPasswordWithConfirmation() should be("e")
    }

    "bail out after x failed tries" in {
      answerQuestionWith("a", "b", "c", "d", "e", "e")
      sut.askPasswordWithConfirmation(maxAttempts = 2) should be(null)

      verify(shellIO, times(4)).readLine(anyString, anyChar)
    }
  }

  "ShellPrompter.askQuestion" should {
    "validate questions" in {
      val question = "Do you like testing?"
      val answer = "answer"

      answerQuestionWith(answer)

      sut.askQuestion(question, List(ShellPromptValidators.nonEmpty)) should equal(answer)
    }

    "support returning a default" in {
      val question = "Do you like testing?"
      val default = "hi!"

      answerQuestionWith("")

      sut.askQuestion(question, List(ShellPromptValidators.nonEmpty), default = default) should equal(default)
    }

    "throw exception if all validation fails" in {
      val question = "Do you like testing?"

      answerQuestionWith("")
      intercept[IllegalArgumentException] {
        sut.askQuestion(question, List(ShellPromptValidators.nonEmpty))
      }
    }

    "throw exception even if only one validator fails" in {
      answerQuestionWith("asdf")

      intercept[IllegalArgumentException] {
        sut.askQuestion("", List(ShellPromptValidators.nonEmpty, _ => ValidationFailure("")))
      }
    }
  }

  "ShellPrompter.pickFromOptions" should {
    "return the input when the answer gives valid number" in {
      answerQuestionWith("1")
      sut.pickFromOptions("", Seq("a", "b", "c")) should be("a")

      answerQuestionWith("2")
      sut.pickFromOptions("", Seq("a", "b", "c")) should be("b")

      answerQuestionWith("3")
      sut.pickFromOptions("", Seq("a", "b", "c")) should be("c")
    }

    "return default if answer doesn't answer" in {
      answerQuestionWith("")
      sut.pickFromOptions("", Seq("a", "b", "c"), default = "b") should be("b")
    }

    "return null if user says 0 and allowNoSelection is true" in {
      answerQuestionWith("0")
      sut.pickFromOptions("", Seq("a", "b", "c"), allowNoSelection = true) should be(null)

      intercept[IllegalArgumentException] {
        sut.pickFromOptions("", Seq("a", "b", "c"), allowNoSelection = false)
      }
    }

    "retry a few times until valid answer" in {
      answerQuestionWith("0", "0", "3")
      sut.pickFromOptions("", Seq("a", "b", "c")) should be("c")
    }
  }

  "ShellPrompter.pickMultipleFromVerboseOptions" should {
    "maintain the order the user entered them" in {
      answerQuestionWith("3,1")
      sut.pickMultipleFromVerboseOptions("", Seq("a", "b", "c"), Seq("aa", "bb", "cc")) should be(Seq("cc", "aa"))
    }

    "return answer if eventually passes" in {
      answerQuestionWith("0", "0", "1,3")
      sut.pickMultipleFromVerboseOptions("", Seq("a", "b", "c"), Seq("aa", "bb", "cc")) should be(Seq("aa", "cc"))
    }

    "return empty if answer contains 0 at all and allowNoSelection is true" in {
      answerQuestionWith("0")
      sut.pickMultipleFromVerboseOptions("", Seq("a", "b", "c"), Seq("aa", "bb", "cc"), allowNoSelection = true) should be(Seq.empty)

      intercept[Exception] {
        sut.pickMultipleFromVerboseOptions("", Seq("a", "b", "c"), Seq("aa", "bb", "cc"), allowNoSelection = false)
      }
    }
  }

  "ShellPrompter.pickMultipleFromOptions" should {
    "maintain the order the user entered them" in {
      answerQuestionWith("3,1")
      sut.pickMultipleFromOptions("", Seq("a", "b", "c")) should be(Seq("c", "a"))
    }

    "return answer if eventually passes" in {
      answerQuestionWith("0", "0", "1,3")
      sut.pickMultipleFromOptions("", Seq("a", "b", "c")) should be(Seq("a", "c"))
    }

    "return empty if answer contains 0 at all and allowNoSelection is true" in {
      answerQuestionWith("0")
      sut.pickMultipleFromOptions("", Seq("a", "b", "c"), allowNoSelection = true) should be(Seq.empty)

      intercept[Exception] {
        sut.pickMultipleFromOptions("", Seq("a", "b", "c"), allowNoSelection = false)
      }
    }
  }

  private def answerQuestionWith(str1: String, str: String*): Unit = {
    when(shellIO.readLine(anyString, anyChar)).thenReturn(str1, str: _*)
  }

  private def feedCharacters(string: String): Unit = {
    reset(shellIO)
    val charArray = string.toCharArray
    when(shellIO.readCharacter(any[Seq[Char]])).thenAnswer(new ReadCharacterAnswer(charArray))
    when(shellIO.readCharacter()).thenReturn(charArray.head, charArray.tail.map(_.toInt): _*)
  }

  private def feedCharacters(string: Char*): Unit = {
    reset(shellIO)
    when(shellIO.readCharacter(any[Seq[Char]])).thenAnswer(new ReadCharacterAnswer(string))
    when(shellIO.readCharacter()).thenReturn(string.head, string.tail.map(_.toInt): _*)
  }

  private var shellIO: ShellIO = _
  private var sut: ShellPrompter = _

  override protected def beforeEach(): Unit = {
    shellIO = mock[ShellIO]
    sut = new ShellPrompter(shellIO)
  }

  private class ReadCharacterAnswer(var available: Seq[Char]) extends Answer[Int] {
    override def answer(invocation: InvocationOnMock): Int = {
      val allowed = invocation.getArguments()(0).asInstanceOf[Seq[Char]]
      while (!allowed.contains(available.head)) {
        available = available.tail
      }
      val result = available.head
      available = available.tail
      result
    }
  }
}
