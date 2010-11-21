import info.whiter4bbit.twitter._
import org.specs._

class base64Test extends SpecificationWithJUnit {
  "byte array must be correctly converted to base64" in {
    TwitterUtil.base64("Man".getBytes) must_== "TWFu"
    TwitterUtil.base64("mama myla ramu".getBytes) must_=="bWFtYSBteWxhIHJhbXU="
  }
}
