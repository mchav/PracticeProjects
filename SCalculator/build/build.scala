import cbt._
import java.net.URL
import java.io.File
import scala.collection.immutable.Seq

class Build( context: Context ) extends BasicBuild( context ){
  override def dependencies = (
    super.dependencies // don't forget super.dependencies here
    ++
    Resolver( mavenCentral ).bind(
      ScalaDependency( "org.scalafx", "scalafx", "8.0.60-R9" ),
      ScalaDependency( "org.scalafx", "scalafxml-core-sfx8", "0.2.2"),
      ScalaDependency( "org.scala-lang.modules", "scala-parser-combinators", "1.0.4")
    )
  )
}
