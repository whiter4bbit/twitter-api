package info.whiter4bbit.twitter

import info.whiter4bbit.twitter._

object TwitterCli {
  def main(args: Array[String]) = {
    val credentials = new AppCredentials(consumerKey = "mFLgUCX686tF8huyEGqA",
                                         consumerSecret = "wgX1wS6ZiAndYwFG1E6OVtP4xrjw7RokOWE6shCaa0M")
    val oauth = new OAuth(credentials)
    val requestToken = oauth.getRequestToken()
    println("Your PIN URL:\nhttp://api.twitter.com/oauth/authorize?oauth_token=" + requestToken.getOauthToken + ":")
    var pin = Console.readLine
    val accessToken = oauth.getAccessToken(pin, requestToken)
    println("We got access token:" + accessToken)
    val statuses = oauth.performGet("http://api.twitter.com/1/statuses/home_timeline.xml", Map(), accessToken)
    println("Statuses:" + statuses)
  }
}
