
/**
 * Contains the configuration for GroupCom middleware.
 * searches config.properties for:
 * port ->   Port number for NameServer
 * host ->   Address to NameServer
 * nsname -> The name of the nameserver inside the registry
 */
import org.slf4j.LoggerFactory
import java.io.FileInputStream
import java.util.Properties

object Config {
  val logger = LoggerFactory.getLogger(this.getClass()); 
  val defaultProps = new Properties();
  //TODO Why is resources not available?
  val in = new FileInputStream("src/resources/config.properties");
  defaultProps.load(in);
	  
  val PORT =   defaultProps.getProperty("port").toInt
  val HOST =   defaultProps.getProperty("host")
  val NSNAME = defaultProps.getProperty("nsname")
  in.close();
}