package dsf.server;

import dsf.core.Message;
import dsf.core.MessageChildTask;
import dsf.core.TaskContext;
import java.util.LinkedList;
import java.util.List;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

/**
 * 服务拨测，每30秒测试服务是否正常
 *
 * @author arksea
 */
public class DialedTask extends MessageChildTask<List<AbstractServiceManager>> {

    public static String TASK_NAME = "dialed_task";
    public static long DEFAULT_TIMEOUT = 10000;
    public static long DIALED_DELAY = 30000;
    public static long STAT_DELAY = 5000;
    private final static Logger logger = LogManager.getLogger(DialedTask.class);

    public static List<ThriftServiceManager> createArgs() {
        return new LinkedList<ThriftServiceManager>();
    }

    public DialedTask(String name, long msgQueuLen, LinkedList<AbstractServiceManager> args) {
        super(name, msgQueuLen, args);
    }

    @Override
    protected void handle_info(Message msg, String from) throws Throwable {
        String key = msg.name;
        if (key.equals("on_stat")) {
            try {
                for (AbstractServiceManager s : state) {
                    s.onStat();
                }
            } finally {
                TaskContext.instance().send_after(STAT_DELAY, TASK_NAME, new Message("on_stat", ""));
            }
        } else if (key.equals("on_dialed")) {
            try {
                for (AbstractServiceManager s : state) {
                    s.onDialed();
                }
            } finally {
                TaskContext.instance().send_after(DIALED_DELAY, TASK_NAME, new Message("on_dialed", ""));
            }
        } else if (key.equals("register")) {
            state.add((AbstractServiceManager) msg.value);
        } else if (key.equals("unregister")) {
            state.remove((AbstractServiceManager) msg.value);
        }
    }

    @Override
    protected void init() throws Throwable {
        TaskContext.instance().send_after(10000, TASK_NAME, new Message("on_dialed", ""));
        TaskContext.instance().send_after(7000, TASK_NAME, new Message("on_stat", ""));
    }

    @Override
    protected void terminate(Throwable ex) {
    }

    @Override
    protected Message handle_call(Message msg, String string) throws Throwable {
        throw new UnsupportedOperationException("Not supported yet.");
    }

}
