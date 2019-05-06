package dsf.register;

import dsf.core.Message;
import com.google.gson.Gson;
import com.google.gson.GsonBuilder;
import com.google.gson.JsonArray;
import com.google.gson.JsonDeserializationContext;
import com.google.gson.JsonDeserializer;
import com.google.gson.JsonElement;
import com.google.gson.JsonObject;
import com.google.gson.JsonParseException;
import com.google.gson.JsonSerializationContext;
import com.google.gson.JsonSerializer;
import java.lang.reflect.Type;
import java.util.HashMap;
import java.util.Map;
import org.apache.logging.log4j.LogManager;

/**
 *
 * @author arksea
 */
public class JsonCodec {

    private GsonBuilder gsonBuilder;
    private final Gson gson;
    private final Map<String, Class> typeMap = new HashMap();
    private final Map<Class, String> classMap = new HashMap();

    public JsonCodec() {
        initTypeMap();
        initGsonBuilder();
        gson = gsonBuilder.create();
    }

    public Message decodec(String msg) throws ClassNotFoundException {
        String[] pair = msg.split("=", 2);
        Class clazz = typeMap.get(pair[0]);
        if (clazz == null) {
            clazz = String.class;
        }
        Object obj = gson.fromJson(pair[1], clazz);
        Message d = new Message(pair[0], obj);
        return d;
    }

    public String encodec(String name, Object obj) {
        String value = gson.toJson(obj, obj.getClass());
        return name + "=" + value;
    }

    public String encodec(Message msg) {
        Object obj = msg.value;
        String value = gson.toJson(obj, obj.getClass());
        return msg.name + "=" + value;
    }

    private void initGsonBuilder() {
        gsonBuilder = new GsonBuilder();
        gsonBuilder.registerTypeAdapter(JsonObject.class, new JsonObjSerializer());
        gsonBuilder.registerTypeAdapter(JsonObject.class, new JsonObjDeserializer());
        gsonBuilder.registerTypeAdapter(JsonArray.class, new JsonArraySerializer());
        gsonBuilder.registerTypeAdapter(JsonArray.class, new JsonArrayDeserializer());
    }

    private class JsonObjSerializer implements JsonSerializer<JsonObject> {

        @Override
        public JsonElement serialize(JsonObject src, Type typeOfSrc, JsonSerializationContext context) {
            return src;
        }
    }

    private class JsonObjDeserializer implements JsonDeserializer<JsonObject> {

        @Override
        public JsonObject deserialize(JsonElement je, Type typeOfT, JsonDeserializationContext context)
                throws JsonParseException {
            return (JsonObject) je;
        }
    }

    private class JsonArraySerializer implements JsonSerializer<JsonArray> {

        @Override
        public JsonElement serialize(JsonArray src, Type typeOfSrc, JsonSerializationContext context) {
            return src;
        }
    }

    private class JsonArrayDeserializer implements JsonDeserializer<JsonArray> {

        @Override
        public JsonArray deserialize(JsonElement je, Type typeOfT, JsonDeserializationContext context)
                throws JsonParseException {
            return (JsonArray) je;
        }
    }

    private void initTypeMap() {
        addTypeMap("string", String.class);
        addTypeMap("notify_svc_state", MsgNotifySvcState.class);
        addTypeMap("subscribe_result", MsgSubscribeResult.class);
        typeMap.put("query_svcdef_result", MsgServiceDefine.class);
        typeMap.put("query_svc_state_result", MsgSubscribeResult.class);
        typeMap.put("notify_svcdef_update", MsgServiceDefine.class);
    }

    private void addTypeMap(String name, Class type) {
        typeMap.put(name, type);
        classMap.put(type, name);
    }

    public static void main(String[] args) {
        try {
            JsonCodec codec = new JsonCodec();
//            String request = "subscribe=\"hello\"";
            
            String request = "notify_svcdef_update={'regname':'com.baidu.dsf.alarm.Alertor v1.0','name':'com.baidu.dsf.alarm.Alertor','version':'1.0','description':'告警收集系统','props':{'protocol':'framed-binary','fail_trategy':'failfast','route_strategy':'roundrobin','timeout':'10000'}}";
            System.out.println(codec.encodec(new Message("test1", "hello")));
            Message d = codec.decodec(request);
            System.out.println(d.name + ":" + d.value.toString());
        } catch (ClassNotFoundException ex) {
            LogManager.getLogger(JsonCodec.class.getName()).error("error", ex);
        }
    }
}
