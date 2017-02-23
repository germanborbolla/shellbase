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

import jline.console.ConsoleReader

/**
  * ShellIO using Console for output and ConsoleReader for input.
  */
class ConsoleShellIO(in: ConsoleReader = new ConsoleReader) extends ShellIO {

  // NOTE(carlton, 2014-01-08): There's probably more stuff we'd like to turn off here;
  // I wish ConsoleReader had a way to say "read a line, disabling all non-interrupt
  // characters" but I don't see it.  At any rate, with this it's safe to have a
  // password with a ! in it.
  in.setExpandEvents(false)

  override def print(x: Any): Unit = {
    Console.print(x)
  }

  override def println(): Unit = {
    Console.println()
  }

  override def println(x: Any): Unit = {
    Console.print(x)
  }

  override def printf(text: String, xs: Any*): Unit = {
    Console.printf(text, xs)
  }

  override def readCharacter(): Int = {
    in.readCharacter()
  }

  override def readCharacter(validChars: Seq[Char]): Int = {
    in.readCharacter(validChars: _*)
  }

  override def readLine(): String = {
    in.readLine()
  }

  override def readLine(maskCharacter: Character): String = {
    in.readLine(maskCharacter)
  }

  override def readLine(prompt: String): String = {
    in.readLine(prompt)
  }

  override def readLine(prompt: String, maskCharacter: Character): String = {
    in.readLine(prompt, maskCharacter)
  }
}
