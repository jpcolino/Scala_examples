val foo = <foo>
  <bar type="greet">hi</bar>
  <bar type="count">1</bar>
  <bar type="color">yellow</bar>
</foo>

foo.text




foo \ "bar"
(foo \ "bar").map(_.text).mkString(" ")
(foo \ "bar").map(_ \ "@type")
(foo \ "bar").map(barNode => (barNode \ "@type", barNode.text))

