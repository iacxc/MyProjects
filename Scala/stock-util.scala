
def hello() {println("hello")}

def findMethods(v: Any) = v.getClass.getMethods.map(_.getName).distinct

