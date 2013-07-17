package jp.ac.keio.flet

import scala.collection.mutable.ListBuffer

object Maildir {
  import scala.util.Random
  import scala.sys.process.Process
  val home_dir = "/home"
  def getRandomFile(input_dir : String, output_dir : String)(implicit max : Int = 0) = {
    val files = Process("ls " + input_dir).lines.toList
    val random_files = (new Random).shuffle(files)
      def getFiles(fs : List[String], i : Int = 0) : List[String] = {
        if (i > max) Nil
        else {
          fs match {
            case h :: t => {
              h :: getFiles(t, i + 1)
            }
            case Nil => Nil
          }
        }
      }
    getFiles(random_files).foreach(f => {
      Process("cp " + input_dir + f + " " + output_dir)!
    })
  }
  def getMailAdress(user : String) {
    val cur_dir = home_dir + user + "/Maildir/cur"
  }
}

class MkWhiteList(val input_dir : String) {
  import scala.collection.mutable.{ Set => MSet }
  import scala.sys.process.Process
  import java.io.PrintWriter
  import scala.util.matching.Regex

  val Email = """(\w+)@([\w\.]+)""".r
  val white_list = exec(input_dir)

  class Mail(val path : String) {
    import javax.mail.Message
    import java.io.FileInputStream
    import java.io.File
    import javax.mail.Session
    import javax.mail.internet.MimeMessage
    import scala.io.Source

    val message = loadMessage(path)

    private[this] def loadMessage(path : String) = {
      val in = new FileInputStream(new File(path))
      val session = Session.getDefaultInstance(new java.util.Properties, null)
      val m = new MimeMessage(session, in)
      in.close
      m
    }
  }

  private[this] def zipDomain(list : List[String], regex : List[Regex], wild_card_domain : List[String]) = wild_card_domain ::: list.filter(x => !regex.map(r => x.matches(r.toString)).foldLeft(false)((p, q) => p || q))
  
  private[this] def checkTopDomain(list : List[String]) : List[String] = {
    val domains = io.Source.fromURL("http://data.iana.org/TLD/tlds-alpha-by-domain.txt").getLines.toList.tail.map(_.toLowerCase)
    list.filter(x => {
      val domain = x.split('.').last
      domains.map(d => if (d == domain) true; else false).foldLeft(false)((p, q) => p || q)
    })
  }

  def exec(input_dir : String) = {
    var set : MSet[String] = MSet.empty
    val file_list = Process("ls " + input_dir).lines.toList
    file_list.foreach(f => {
      val mail = new Mail(input_dir + "/" + f)
      val cc = mail.message.getHeader("CC")
      val to = mail.message.getHeader("To")
      val send = (cc, to) match {
        case (null, null) => Nil
        case (null, t)    => t.toList
        case (c, null)    => c.toList
        case (c, t)       => t.toList ::: c.toList
      }
      val tc = send.flatMap(x => x.split(',')).map(_.trim.trim) //平滑化
      val c = tc.map(x => {
        val mail_adder = Email.findFirstIn(x).map { set.add }.getOrElse(println(x + " is bad"))
      })
    })

    val zippedDomain = zipDomain(set.toList.filter(x => x.last != '.'), List("""(\w+)@([\w\.]*)keio.ac.jp""".r, """(\w+)@([\w\.]*)keio.jp""".r), List("*.keio.ac.jp", "*keio.jp"))
    checkTopDomain(zippedDomain)
  }

  def write(out_file : String) = {
    val out = new PrintWriter(out_file)
    white_list.foreach(x => out.println("whitelist_from " + x))
    out.close
  }

  def count = {
    import scala.collection.mutable.ListBuffer
    import scala.collection.mutable.Map
    var lb : ListBuffer[(String, Int)] = ListBuffer.empty
      def c(l : List[String]) : Unit = {
        l match {
          case h :: t => {
            val i = lb.toList.indexWhere(x => x._1 == h)
            if (i != -1) lb(i) = (lb(i)._1, lb(i)._2 + 1)
            else lb.append((h, 0))
            c(t)
          }
          case Nil =>
        }
      }
    c(white_list.map(_.split('@').last))
    lb.toList
  }
}

object Main extends App {
  import scala.collection.mutable.ListBuffer
  import scala.collection.mutable.Map
  var lb : ListBuffer[(String, Int)] = ListBuffer.empty
  def c(l : List[String]) : Unit = {
    l match {
      case h :: t => {
        val i = lb.toList.indexWhere(x => x._1 == h)
        if (i != -1) lb(i) = (lb(i)._1, lb(i)._2 + 1)
        else lb.append((h, 0))
        c(t)
      }
      case Nil =>
    }
  }
}