package sbinary;

trait DefaultProtocol extends StandardTypes with JavaIOProtocol with JavaFormats;
object DefaultProtocol extends DefaultProtocol;
