/*
 * MIT License
 *
 * Copyright (c) 2019 Hrvoje Peradin
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all
 * copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 * SOFTWARE.
 *
 */

package krvoje.znjtorrent.tracker

import java.net.URLEncoder

case class TrackerRequest(
                           infoHash  : String,
                           peerID    : String,
                           port      : Int,
                           uploaded  : BigInt,
                           downloaded: BigInt,
                           left      : BigInt,
                           compact   : Boolean,
                           noPeerID  : Boolean,
                           event     : TrackerEvent,
                           ip        : String,
                           numWant   : Int,
                           key       : String,
                           trackerID : String) {

  def uriString: String = URLEncoder.encode(requestParams.map(_._1).mkString("&"), "utf-8")

  def requestParams = Seq(
    ("info_hash" -> infoHash),
    ("peer_id" -> peerID),
    ("port" -> port.toString),
    ("uploaded" -> uploaded.toString),
    ("downloaded" -> downloaded.toString),
    ("left" -> left.toString),
    ("compact" -> (if (compact) "1" else "0")),
    ("noPeerID" -> (if (noPeerID) "1" else "0")),
    ("event" -> event.value),
    ("ip" -> ip),
    ("numwant" -> numWant.toString),
    ("key" -> key),
    ("trackerid" -> trackerID))
}







