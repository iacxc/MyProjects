
trait HadoopUtil {
    val prefix = "http"
    val host: String
    val port: Int

    def baseurl = s"$prefix://$host:$port"

}

class Hdfs(h: String, user: String) extends HadoopUtil {
    val host = h
    val port = 50070

    lazy val userspec = "&user.name=" + user

    def ls(dirname: String) = 
        baseurl + s"/webhdfs/v1$dirname?op=LISTSTATUS$userspec"

    def mkdir(dirname: String, perm: String = "777") = 
        baseurl + s"/webhdfs/v1$dirname?op=MKDIRS&permission=$perm$userspec"

    def create(filename: String) = 
        baseurl + s"/webhdfs/v1$filename?op=CREATE$userspec"

    def cp(localfile: String, filename: String) =
        baseurl + s"/webhdfs/v1$filename?op=CREATE$userspec"

    def delete(filename: String) = 
        baseurl + s"/webhdfs/v1$filename?op=DELETE$userspec"

    def cat(filename: String) = 
        baseurl + s"/webhdfs/v1$filename?op=OPEN$userspec"

    def rename(srcname: String, destname: String) =
        baseurl + "/webhdfs/v1" + srcname + 
                  "?op=RENAME&destination=" + destname + 
                  userspec
}

object Hdfs {
    def apply(host: String, user: String = "hdfs") = new Hdfs(host, user)
}

/*
class Knox(h: String, user: String, password: String) extends Hadoop {
    override val prefix = "https"
    override val host = h
    override val port = 8443

    val hdfs = Hdfs(host, user)
    override val rooturl = "gateway/default/" + hdfs.rooturl

    def ls(dirname: String) = weburl + dirname + "?op=LISTSTATUS"

    def mkdir(dirname: String) = weburl + dirname + "?op=MKDIR&permission=777"

    def cat(file: String) = weburl + file + "?op=OPEN"
}

object Knox {
    def apply(host: String, user: String, password: String) =
        new Knox(host, user, password)
}

*/

object HadoopTest extends App {
    val hdfs = Hdfs("adl03", "caiche")
    println(hdfs.ls("/user/caiche"))
    println(hdfs.mkdir("/user/caiche/test", "700"))
    println(hdfs.cat("/user/caiche/test.txt"))
}
