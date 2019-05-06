package dsf.thrift;

import java.util.zip.DataFormatException;
import java.util.zip.Deflater;
import java.util.zip.Inflater;
import org.apache.thrift.transport.TMemoryInputTransport;
import org.apache.thrift.transport.TTransport;
import org.apache.thrift.transport.TTransportException;
import org.apache.thrift.TByteArrayOutputStream;
import org.apache.thrift.transport.TTransportFactory;

/**
 *
 * @author arksea
 */
public class TZlibTransport extends TTransport {

    protected static final int DEFAULT_MAX_LENGTH = 16384000;
    private int maxLength_;
    private Deflater compresser = new Deflater();
    private byte[] compBuf = new byte[512];
    private final byte[] i32buf = new byte[4];
    private Inflater decompresser = new Inflater();
    /**
     * Underlying transport
     */
    private TTransport transport_ = null;
    /**
     * Buffer for output
     */
    private final TByteArrayOutputStream writeBuffer_ =
            new TByteArrayOutputStream(1024);
    /**
     * Buffer for input
     */
    private TMemoryInputTransport readBuffer_ = new TMemoryInputTransport(new byte[4096],0,0);

    public static class Factory extends TTransportFactory {

        private int maxLength_;

        public Factory() {
            maxLength_ = TZlibTransport.DEFAULT_MAX_LENGTH;
        }

        public Factory(int maxLength) {
            maxLength_ = maxLength;
        }

        @Override
        public TTransport getTransport(TTransport base) {
            return new TZlibTransport(base, maxLength_);
        }
    }

    /**
     * Constructor wraps around another transport
     */
    public TZlibTransport(TTransport transport, int maxLength) {
        transport_ = transport;
        maxLength_ = maxLength;
    }

    public TZlibTransport(TTransport transport) {
        transport_ = transport;
        maxLength_ = TZlibTransport.DEFAULT_MAX_LENGTH;
    }

    @Override
    public void open() throws TTransportException {
        transport_.open();
    }

    @Override
    public boolean isOpen() {
        return transport_.isOpen();
    }

    @Override
    public void close() {
        compresser.end();
        decompresser.end();
        transport_.close();
    }

    @Override
    public int read(byte[] buf, int off, int len) throws TTransportException {
        if (readBuffer_ != null) {
            int got = readBuffer_.read(buf, off, len);
            if (got > 0) {
                return got;
            }
        }

        // Read another frame of data
        readFrame();

        return readBuffer_.read(buf, off, len);
    }

    @Override
    public byte[] getBuffer() {
        return readBuffer_.getBuffer();
    }

    @Override
    public int getBufferPosition() {
        return readBuffer_.getBufferPosition();
    }

    @Override
    public int getBytesRemainingInBuffer() {
        return readBuffer_.getBytesRemainingInBuffer();
    }

    @Override
    public void consumeBuffer(int len) {
        readBuffer_.consumeBuffer(len);
    }

    private void readFrame() throws TTransportException {
        transport_.readAll(i32buf, 0, 4);
        int size = decodeFrameSize(i32buf);
        if (size < 0) {
            throw new TTransportException("Read a negative frame size (" + size + ")!");
        }
        if (size > maxLength_) {
            throw new TTransportException("Frame size (" + size + ") larger than max length (" + maxLength_ + ")!");
        }
        byte[] buff = new byte[size];
        transport_.readAll(buff, 0, size);
        byte[] decompBuf = readBuffer_.getBuffer();
        try {
            decompresser.reset();
            decompresser.setInput(buff, 0, size);
            int n = decompresser.inflate(decompBuf);
            while (!decompresser.needsInput()) {
                byte[] copy = new byte[decompBuf.length*2];
                System.arraycopy(decompBuf, 0, copy, 0, n);
                decompBuf = copy;
                n += decompresser.inflate(decompBuf, n, decompBuf.length-n);
            }
            readBuffer_.reset(decompBuf,0,n);
        } catch (DataFormatException ex) {
            throw new TTransportException("compressed data formate error", ex);
        }
    }

    @Override
    public void write(byte[] buf, int off, int len) throws TTransportException {
        writeBuffer_.write(buf, off, len);
    }
    @Override
    public void flush() throws TTransportException {
        byte[] buf = writeBuffer_.get();
        int len = writeBuffer_.len();
        writeBuffer_.reset();

        compresser.reset();
        compresser.setInput(buf,0,len);
        compresser.finish();  //data stream end
        int n = compresser.deflate(compBuf);
        int copyCount = 0;
        while(!compresser.finished()) {
            ++copyCount;
            byte[] copy = new byte[compBuf.length*2];
            System.arraycopy(compBuf, 0, copy, 0, n);
            compBuf = copy;
            n += compresser.deflate(compBuf, n, compBuf.length-n);
        }
        encodeFrameSize(n, i32buf);
        //System.out.println("write: RawSize="+len+"; ZipedSize="+n);
        transport_.write(i32buf, 0, 4);
        transport_.write(compBuf, 0, n);
        transport_.flush();
    }

    public static final void encodeFrameSize(final int frameSize, final byte[] buf) {
        buf[0] = (byte) (0xff & (frameSize >> 24));
        buf[1] = (byte) (0xff & (frameSize >> 16));
        buf[2] = (byte) (0xff & (frameSize >> 8));
        buf[3] = (byte) (0xff & (frameSize));
    }

    public static final int decodeFrameSize(final byte[] buf) {
        return ((buf[0] & 0xff) << 24) |
                ((buf[1] & 0xff) << 16) |
                ((buf[2] & 0xff) << 8) |
                ((buf[3] & 0xff));
    }
}
