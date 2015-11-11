
trait HadoopUtil {
    val prefix = "http"
    val host: String
    val port: Int

    def baseurl = "%s://%s:%s".format(prefix, host, port)

}

object Curl{
    private def curl(opt: String) = 
        "curl -L" + (if (opt.length > 0) " " + opt else "")

    def Get(url: String, curlopt: String="") = 
        curl(curlopt) + " '" + url + "'"
    def Put(url: String, curlopt: String="") = 
        curl(curlopt) + " -X PUT '" + url + "'"
    def Delete(url: String, curlopt: String="") = 
        curl(curlopt) + " -X DELETE '" + url + "'"
    def Post(url: String, curlopt: String="") = 
        curl(curlopt) + " -X POST '" + url + "'"
}

class Hdfs(h: String, user: String) extends HadoopUtil {
    val host = h
    val port = 50070

    lazy val userspec = "&user.name=" + user
    lazy val weburl = baseurl + "/webhdfs/v1"

    import Curl._
    def ls(dirname: String) = 
        Get(weburl + dirname + "?op=LISTSTATUS" + userspec)

    def mkdir(dirname: String, perm: String="777") = 
        Put(weburl + dirname +
            "?op=MKDIRS&permission=" + perm + userspec)

    def create(filename: String) = 
        Put(weburl + filename +"?op=CREATE" + userspec)

    def cp(localfile: String, filename: String) =
        Put(weburl + filename + "?op=CREATE" + userspec,
            "-T " + localfile)

    def append(localfile: String, filename: String) =
        Put(weburl + filename + "?op=APPEND" + userspec,
            "-T " + localfile)

    def delete(filename: String) = 
        Delete(weburl + filename + "?op=DELETE" + userspec)

    def stat(filename: String) = 
        Get(weburl + filename + "?op=GETFILESTATUS" + userspec)

    def cat(filename: String) = 
        Get(weburl + filename + "?op=OPEN" + userspec)

    def rename(srcname: String, destname: String) =
        Put(weburl + "/webhdfs/v1" + srcname + 
            "?op=RENAME&destination=" + destname + userspec)

    def chmod(filename: String, perm: String="700") =
        Put(weburl + filename + 
            "?op=SETPERMISSION&permission=" + perm + userspec)

    def chown(filename: String, owner: String, group: String="") = 
        Put(weburl + filename +
            "?op=SETOWNER&owner=" + owner +
            (if (group.length > 0) "&group=" + group else "") +
            userspec)

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
    val host = if (args.length > 0) args(0) else "localhost"
    val hdfs = Hdfs(host, "caiche")
    println(hdfs.ls("/user/caiche"))
    println(hdfs.mkdir("/user/caiche/test", "700"))
    println(hdfs.cp("hadoop.scala", "/user/caiche/test.1"))
    println(hdfs.append("kdfs.cmd1", "/user/caiche/test.2"))
    println(hdfs.chown("/user/caiche/test.3", "caiche", "caiche"))
    println(hdfs.chmod("/user/caiche/test.4", "700"))
    println(hdfs.stat("/user/caiche/test.2"))
    println(hdfs.cat("/user/caiche/test.2"))
}
