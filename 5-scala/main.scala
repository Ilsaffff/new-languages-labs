import java.io._
import java.net._
import com.sun.net.httpserver.{HttpServer, HttpHandler, HttpExchange}

import scala.collection.mutable.ListBuffer
import scala.io.Source

case class User(id: Int, name: String, age: Int)

object RestApiExample {

  var users = ListBuffer(
    User(1, "Alice", 30),
    User(2, "Bob", 25),
    User(3, "Charlie", 28)
  )

  def main(args: Array[String]): Unit = {
    val server = HttpServer.create(new InetSocketAddress(8080), 0)

    server.createContext("/users", new UsersHandler())
    server.createContext("/users/add", new AddUserHandler())
    server.createContext("/users/", new UserByIdHandler())

    server.setExecutor(null)
    server.start()

    println("Server started at http://localhost:8080/")
  }

  class UsersHandler extends HttpHandler {
    override def handle(exchange: HttpExchange): Unit = {
      val response = users.mkString("\n")
      exchange.sendResponseHeaders(200, response.getBytes.length)
      val os = exchange.getResponseBody
      os.write(response.getBytes)
      os.close()
    }
  }

  import scala.util.Try

  class AddUserHandler extends HttpHandler {
    override def handle(exchange: HttpExchange): Unit = {
      val method = exchange.getRequestMethod
      if (method == "POST") {
        try {
          val inputStream = exchange.getRequestBody
          val requestBody =
            scala.io.Source.fromInputStream(inputStream).mkString

          val user = parseJsonToUser(requestBody)

          user match {
            case Some(u) =>
              users += u
              val response = s"User ${u.name} added successfully"
              exchange.sendResponseHeaders(200, response.getBytes.length)
              val os = exchange.getResponseBody
              os.write(response.getBytes)
              os.close()
            case None =>
              val response = "Invalid input data."
              exchange.sendResponseHeaders(400, response.getBytes.length)
              val os = exchange.getResponseBody
              os.write(response.getBytes)
              os.close()
          }
        } catch {
          case ex: Exception =>
            val response = s"Error processing request: ${ex.getMessage}"
            exchange.sendResponseHeaders(500, response.getBytes.length)
            val os = exchange.getResponseBody
            os.write(response.getBytes)
            os.close()
        }
      } else {
        val response = "Only POST method is allowed."
        exchange.sendResponseHeaders(405, response.getBytes.length)
        val os = exchange.getResponseBody
        os.write(response.getBytes)
        os.close()
      }
    }

    private def parseJsonToUser(json: String): Option[User] = {
      val trimmedJson = json.trim
      if (trimmedJson.startsWith("{") && trimmedJson.endsWith("}")) {
        val content = trimmedJson.stripPrefix("{").stripSuffix("}")
        val keyValuePairs = content
          .split(",")
          .map(_.split(":").map(_.trim.stripPrefix("\"").stripSuffix("\"")))
        val jsonMap = keyValuePairs.collect { case Array(k, v) => k -> v }.toMap

        for {
          id <- Try(jsonMap.getOrElse("id", "").toInt).toOption
          name <- jsonMap.get("name")
          age <- Try(jsonMap.getOrElse("age", "").toInt).toOption
        } yield User(id, name, age)
      } else {
        None
      }
    }
  }

  class UserByIdHandler extends HttpHandler {
    override def handle(exchange: HttpExchange): Unit = {
      val path = exchange.getRequestURI.getPath
      val id = path.substring(path.lastIndexOf("/") + 1).toInt

      val userOption = users.find(_.id == id)

      userOption match {
        case Some(user) =>
          val response = s"User: ${user.name}, Age: ${user.age}"
          exchange.sendResponseHeaders(200, response.getBytes.length)
          val os = exchange.getResponseBody
          os.write(response.getBytes)
          os.close()
        case None =>
          val response = s"User with ID $id not found"
          exchange.sendResponseHeaders(404, response.getBytes.length)
          val os = exchange.getResponseBody
          os.write(response.getBytes)
          os.close()
      }
    }
  }
}
