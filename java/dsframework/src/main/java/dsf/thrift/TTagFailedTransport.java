package dsf.thrift;

import org.apache.thrift.transport.TTransport;
import org.apache.thrift.transport.TTransportException;

/**
 * @author arksea
 * 用于标记失败连接，catch所有TransportException,打上标记后重新抛出，
 * 连接池将使用这个标记丢弃失败的连接。
 * 此类非线程安全，所以获取的Client必须由同一个线程调用及归还到连接池
 */
public class TTagFailedTransport extends TTransport {

    private final TTransport transport;
    private boolean __failed = false; //双下划线是为了提醒必须通过getter、setter使用
    private Throwable __failedException = null;

    public TTagFailedTransport(TTransport transport) {
        this.transport = transport;
    }

    public synchronized boolean isFailed() {
        return __failed;
    }

    public synchronized Throwable getFailedException() {
        return __failedException;
    }

    private synchronized void failed(Throwable ex) {
        __failed = true;
        __failedException = ex;
    }

    @Override
    public boolean isOpen() {
        return transport.isOpen();
    }

    @Override
    public boolean peek() {
        return transport.peek();
    }

    @Override
    public void open() throws TTransportException {
        try {
            transport.open();
        } catch (TTransportException ex) {
            failed(ex);
            throw ex;
        }
    }

    @Override
    public void close() {
        transport.close();
    }

    @Override
    public int read(byte[] buf, int off, int len) throws TTransportException {
        try {
            return transport.read(buf, off, len);
        } catch (TTransportException ex) {
            failed(ex);
            throw ex;
        }
    }

    @Override
    public int readAll(byte[] buf, int off, int len) throws TTransportException {
        try {
            return transport.readAll(buf, off, len);
        } catch (TTransportException ex) {
            failed(ex);
            throw ex;
        }
    }

    @Override
    public void write(byte[] buf) throws TTransportException {
        try {
            transport.write(buf);
        } catch (TTransportException ex) {
            failed(ex);
            throw ex;
        }
    }

    @Override
    public void write(byte[] buf, int off, int len) throws TTransportException {
        try {
            transport.write(buf, off, len);
        } catch (TTransportException ex) {
            failed(ex);
            throw ex;
        }
    }

    @Override
    public void flush() throws TTransportException {
        try {
            transport.flush();
        } catch (TTransportException ex) {
            failed(ex);
            throw ex;
        }
    }

    @Override
    public byte[] getBuffer() {
        return transport.getBuffer();
    }

    @Override
    public int getBufferPosition() {
        return transport.getBufferPosition();
    }

    @Override
    public int getBytesRemainingInBuffer() {
        return transport.getBytesRemainingInBuffer();
    }

    @Override
    public void consumeBuffer(int len) {
        transport.consumeBuffer(len);
    }
    
}
