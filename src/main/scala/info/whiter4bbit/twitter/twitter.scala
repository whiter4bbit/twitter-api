package info.whiter4bbit.twitter

import java.net.URLEncoder
import java.security.SignatureException
import javax.crypto.Mac
import javax.crypto.spec.SecretKeySpec
import java.util.Arrays
import java.net._
import java.io._

class AppCredentials(consumerKey: String, consumerSecret: String) {
  def getConsumerKey = this.consumerKey
  def getConsumerSecret = this.consumerSecret
  override def toString = "applicationCredentials: consumerKey:'" + consumerKey + "' consumerSecret:'" + consumerSecret + "'"
}

class Token(oauthToken: String, oauthTokenSecret: String) {
  def getOauthToken = this.oauthToken
  def getOauthTokenSecret = this.oauthTokenSecret
  override def toString = "oauthToken '" + oauthToken + "' oauthTokenSecret'" + oauthTokenSecret + "'"
}

class OAuth(credentials: AppCredentials) {
  val tokenRequestURL = "http://api.twitter.com/oauth/request_token"
  val accessTokenRequestURL = "https://api.twitter.com/oauth/access_token"
  val timestamp = System.currentTimeMillis() / 1000
  val oauthNonce = System.nanoTime.toString
  val defaultRequestParameters = Map("oauth_consumer_key"->credentials.getConsumerKey,
                                     "oauth_callback"->"oob",
                                     "oauth_nonce"->oauthNonce,
                                     "oauth_signature_method"->"HMAC-SHA1",
                                     "oauth_timestamp"->timestamp.toString)

  def parseToken(response: String): Token = {
    var oauthToken = ""
    var oauthTokenSecret = ""
    for (param <- response.split("&")) {
      val pair = param.split("=")
      if (pair(0)=="oauth_token") oauthToken = pair(1)
      if (pair(0)=="oauth_token_secret") oauthTokenSecret = pair(1)
    }
    return new Token(oauthToken, oauthTokenSecret)
  }

  def getRequestToken(): Token = {
    var authRequestParameters = defaultRequestParameters
    parseToken(doPost(tokenRequestURL, authRequestParameters, None))
  }

  def getAccessToken(pin: String, requestToken: Token): Token =  {
    var authRequestParameters = defaultRequestParameters ++
                                Map("oauth_token"->requestToken.getOauthToken,
                                    "oauth_verifier"->pin)
    val response = doPost(accessTokenRequestURL, authRequestParameters, Some(requestToken))
    parseToeken(response)
  }

  def performGet(requestURL: String, params: Map[String, String], requestToken: Option[Token]) = {
    doGet(requestURL, params ++ defaultRequestParameters ++
                     (requestToken match {
                       case Some(token) => Map("oauth_token"->token.getOauthToken)
                       case None => Map()
                     }), requestToken)
  }

  private def wrap(source: String) = "\"" + source + "\""

  private def doGet(requestURL: String, params: Map[String, String], requestToken: Option[Token]) =
    doFetch(requestURL, params, requestToken, "GET")

  private def doPost(requestURL: String, params: Map[String, String], requestToken: Option[Token]) =
    doFetch(requestURL, params, requestToken, "POST")

  private def doFetch(requestURL: String, params: Map[String, String], requestToken: Option[Token], method: String) = {
    var oauthData = TwitterUtil.encode(params, requestURL , method)
    val secretKey = credentials.getConsumerSecret + "&" +
                    (requestToken match {
                       case Some(token) => token.getOauthTokenSecret
                       case None => ""
                    })
    var oauthSignature = TwitterUtil.hmac(oauthData, secretKey)
    val headerParameters = params.map(e=>(e._1,wrap(e._2))) ++
                           Map("oauth_signature"->wrap(oauthSignature))
    var authHeader = "OAuth " + TwitterUtil.encodeParameters(headerParameters, ",")
    fetchURL(requestURL, Map("Authorization"->authHeader), method)
  }

  private def fetchURL(requestURL: String, headers: Map[String, String], method: String): String = {
    var url = new URL(requestURL)
    var connection = url.openConnection().asInstanceOf[HttpURLConnection]
    connection.setRequestMethod(method)
    connection.setDoOutput(true)
    for (header<-headers) {
       connection.setRequestProperty(header._1, header._2)
    }
    val is = connection.getInputStream
    var response = ""
    try{
      response = scala.io.Source.fromInputStream(is).mkString
    } finally {
      is.close
    }
    response
  }
}
