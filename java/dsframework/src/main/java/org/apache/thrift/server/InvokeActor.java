package org.apache.thrift.server;

import akka.actor.Props;
import akka.actor.UntypedActor;
import akka.event.Logging;
import akka.event.LoggingAdapter;
import akka.japi.Creator;
import org.apache.thrift.TProcessor;
import scala.Option;

/**
 *
 * @author arksea
 */
public class InvokeActor extends UntypedActor {
    private final LoggingAdapter log = Logging.getLogger(this);
    private final TProcessor processor;
    
    public InvokeActor(TProcessor processor) throws Exception {
        this.processor = processor;
    }

    public static Props props(final TProcessor processor) {
        return Props.create(new Creator<InvokeActor>() {
            @Override
            public InvokeActor create() throws Exception {
                return new InvokeActor(processor);
            }
        });
    }

    @Override
    public void preStart() throws Exception {
        log.debug("InvokeActor:preStart");
    }

    @Override
    public void preRestart(Throwable thrwbl, Option<Object> option) throws Exception {
    }

    @Override
    public void postRestart(Throwable thrwbl) throws Exception {
    }

    @Override
    public void postStop() throws Exception {
        log.debug("InvokeActor:postStop");
    }

    @Override
    public void onReceive(Object msg) throws Exception {
        if (msg instanceof AbstractNonblockingServer.FrameBuffer) {
            ((AbstractNonblockingServer.FrameBuffer)msg).invoke();
        } else if (msg instanceof Runnable) {
            ((Runnable)msg).run();
        }
    }

}
