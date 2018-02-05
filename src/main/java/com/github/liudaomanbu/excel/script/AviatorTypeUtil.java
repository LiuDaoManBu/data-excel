package com.github.liudaomanbu.excel.script;

import java.util.Arrays;
import java.util.List;
import java.util.Map;
import com.github.liudaomanbu.excel.script.constant.ScriptType;
import com.github.liudaomanbu.excel.script.expression.ScriptExpression;
import com.github.liudaomanbu.excel.script.runtime.function.ScriptFunction;
import com.github.liudaomanbu.excel.script.runtime.type.ScriptObject;
import com.google.common.collect.BiMap;
import com.google.common.collect.ImmutableBiMap;
import com.googlecode.aviator.Expression;
import com.googlecode.aviator.parser.AviatorClassLoader;
import com.googlecode.aviator.runtime.type.AviatorFunction;
import com.googlecode.aviator.runtime.type.AviatorObject;
import com.googlecode.aviator.runtime.type.AviatorType;

class AviatorTypeUtil {
  private static final BiMap<ScriptType, AviatorType> SCRIPT_TYPE_TO_AVIATOR_TYPES =
      ImmutableBiMap.<ScriptType, AviatorType>builder().put(ScriptType.DOUBLE, AviatorType.Double)
          .put(ScriptType.DECIMAL, AviatorType.Decimal).put(ScriptType.LONG, AviatorType.Long)
          .put(ScriptType.STRING, AviatorType.String).put(ScriptType.JAVATYPE, AviatorType.JavaType)
          .put(ScriptType.BOOLEAN, AviatorType.Boolean).put(ScriptType.PATTERN, AviatorType.Pattern)
          .put(ScriptType.Nil, AviatorType.Nil).put(ScriptType.METHOD, AviatorType.Method)
          .put(ScriptType.BIGINT, AviatorType.BigInt).build();
  private static final Map<AviatorType, ScriptType> AVIATOR_TYPE_TO_SCRIPT_TYPES =
      SCRIPT_TYPE_TO_AVIATOR_TYPES.inverse();

  public static ScriptType to(AviatorType aviatorType) {
    return AVIATOR_TYPE_TO_SCRIPT_TYPES.get(aviatorType);
  }

  public static AviatorType to(ScriptType scriptType) {
    return SCRIPT_TYPE_TO_AVIATOR_TYPES.get(scriptType);
  }

  public static ScriptFunction to(AviatorFunction function) {
    return AviatorScriptFunction.to(function);
  }

  public static AviatorFunction to(ScriptFunction function) {
    return ScriptAviatorFunction.to(function);
  }

  public static ScriptClassLoader to(AviatorClassLoader classLoader) {
    return AviatorScriptClassLoader.to(classLoader);
  }

  public static ScriptExpression to(com.googlecode.aviator.Expression expression) {
    return AviatorScriptExpression.to(expression);
  }

  public static ScriptObject to(AviatorObject aviatorObject) {
    return AviatorScriptObject.to(aviatorObject);
  }

  public static ScriptObject[] to(AviatorObject... aviatorObjects) {
    return Arrays.stream(aviatorObjects).map(AviatorTypeUtil::to).toArray(ScriptObject[]::new);
  }

  public static AviatorObject to(ScriptObject scriptObject) {
    return ScriptAviatorObject.to(scriptObject);
  }

  public static AviatorObject[] to(ScriptObject... scriptObjects) {
    return Arrays.stream(scriptObjects).map(AviatorTypeUtil::to).toArray(AviatorObject[]::new);
  }

  private static class ScriptAviatorObject extends AviatorObject {
    private static ScriptAviatorObject to(ScriptObject scriptObject) {
      return new ScriptAviatorObject(scriptObject);
    }

    private final ScriptObject scriptObject;
    
    private ScriptAviatorObject(ScriptObject scriptObject) {
      super();
      this.scriptObject = scriptObject;
    }

    @Override
    public int compare(AviatorObject other, Map<String, Object> env) {
      return scriptObject.compare(AviatorTypeUtil.to(other), env);
    }

    @Override
    public AviatorType getAviatorType() {
      return AviatorTypeUtil.to(scriptObject.getScriptType());
    }

    @Override
    public boolean isNull(Map<String, Object> env) {
      return scriptObject.isNull(env);
    }

    @Override
    public AviatorObject match(AviatorObject other, Map<String, Object> env) {
      return to(scriptObject.match(AviatorTypeUtil.to(other), env));
    }

    @Override
    public AviatorObject neg(Map<String, Object> env) {
      return to(scriptObject.neg(env));
    }

    @Override
    public AviatorObject not(Map<String, Object> env) {
      return to(scriptObject.not(env));
    }

    @Override
    public String desc(Map<String, Object> env) {
      return scriptObject.desc(env);
    }

    @Override
    public Object getValue(Map<String, Object> env) {
      return scriptObject.getValue(env);
    }

    @Override
    public AviatorObject add(AviatorObject other, Map<String, Object> env) {
      return to(scriptObject.add(AviatorTypeUtil.to(other), env));
    }

    @Override
    public AviatorObject bitAnd(AviatorObject other, Map<String, Object> env) {
      return to(scriptObject.bitAnd(AviatorTypeUtil.to(other), env));
    }

    @Override
    public AviatorObject bitOr(AviatorObject other, Map<String, Object> env) {
      return to(scriptObject.bitOr(AviatorTypeUtil.to(other), env));
    }

    @Override
    public AviatorObject bitXor(AviatorObject other, Map<String, Object> env) {
      return to(scriptObject.bitXor(AviatorTypeUtil.to(other), env));
    }

    @Override
    public AviatorObject shiftRight(AviatorObject other, Map<String, Object> env) {
      return to(scriptObject.shiftRight(AviatorTypeUtil.to(other), env));
    }

    @Override
    public AviatorObject shiftLeft(AviatorObject other, Map<String, Object> env) {
      return to(scriptObject.shiftLeft(AviatorTypeUtil.to(other), env));
    }

    @Override
    public AviatorObject unsignedShiftRight(AviatorObject other, Map<String, Object> env) {
      return to(scriptObject.unsignedShiftRight(AviatorTypeUtil.to(other), env));
    }

    @Override
    public AviatorObject bitNot(Map<String, Object> env) {
      return to(scriptObject.bitNot(env));
    }

    @Override
    public AviatorObject sub(AviatorObject other, Map<String, Object> env) {
      return to(scriptObject.sub(AviatorTypeUtil.to(other), env));
    }

    @Override
    public AviatorObject mod(AviatorObject other, Map<String, Object> env) {
      return to(scriptObject.mod(AviatorTypeUtil.to(other), env));
    }

    @Override
    public AviatorObject div(AviatorObject other, Map<String, Object> env) {
      return to(scriptObject.div(AviatorTypeUtil.to(other), env));
    }

    @Override
    public AviatorObject mult(AviatorObject other, Map<String, Object> env) {
      return to(scriptObject.mult(AviatorTypeUtil.to(other), env));
    }

    @Override
    public Number numberValue(Map<String, Object> env) {
      return scriptObject.numberValue(env);
    }

    @Override
    public String stringValue(Map<String, Object> env) {
      return scriptObject.stringValue(env);
    }

    @Override
    public boolean booleanValue(Map<String, Object> env) {
      return scriptObject.booleanValue(env);
    }

    @Override
    public AviatorObject getElement(Map<String, Object> env, AviatorObject indexObject) {
      return to(scriptObject.getElement(env, AviatorTypeUtil.to(indexObject)));
    }
  }

  private static class AviatorScriptObject extends ScriptObject {

    public static AviatorScriptObject to(AviatorObject aviatorObject) {
      return new AviatorScriptObject(aviatorObject);
    }

    private final AviatorObject aviatorObject;
    
    private AviatorScriptObject(AviatorObject aviatorObject) {
      super();
      this.aviatorObject = aviatorObject;
    }

    @Override
    public int compare(ScriptObject other, Map<String, Object> env) {
      return aviatorObject.compare(AviatorTypeUtil.to(other), env);
    }

    @Override
    public ScriptType getScriptType() {
      return AviatorTypeUtil.to(aviatorObject.getAviatorType());
    }

    @Override
    public boolean isNull(Map<String, Object> env) {
      return aviatorObject.isNull(env);
    }

    @Override
    public ScriptObject match(ScriptObject other, Map<String, Object> env) {
      return to(aviatorObject.match(AviatorTypeUtil.to(other), env));
    }

    @Override
    public ScriptObject neg(Map<String, Object> env) {
      return to(aviatorObject.neg(env));
    }

    @Override
    public ScriptObject not(Map<String, Object> env) {
      return to(aviatorObject.not(env));
    }

    @Override
    public String desc(Map<String, Object> env) {
      return aviatorObject.desc(env);
    }

    @Override
    public Object getValue(Map<String, Object> env) {
      return aviatorObject.getValue(env);
    }

    @Override
    public ScriptObject add(ScriptObject other, Map<String, Object> env) {
      return to(aviatorObject.add(AviatorTypeUtil.to(other), env));
    }

    @Override
    public ScriptObject bitAnd(ScriptObject other, Map<String, Object> env) {
      return to(aviatorObject.bitAnd(AviatorTypeUtil.to(other), env));
    }

    @Override
    public ScriptObject bitOr(ScriptObject other, Map<String, Object> env) {
      return to(aviatorObject.bitOr(AviatorTypeUtil.to(other), env));
    }

    @Override
    public ScriptObject bitXor(ScriptObject other, Map<String, Object> env) {
      return to(aviatorObject.bitXor(AviatorTypeUtil.to(other), env));
    }

    @Override
    public ScriptObject shiftRight(ScriptObject other, Map<String, Object> env) {
      return to(aviatorObject.shiftRight(AviatorTypeUtil.to(other), env));
    }

    @Override
    public ScriptObject shiftLeft(ScriptObject other, Map<String, Object> env) {
      return to(aviatorObject.shiftLeft(AviatorTypeUtil.to(other), env));
    }

    @Override
    public ScriptObject unsignedShiftRight(ScriptObject other, Map<String, Object> env) {
      return to(aviatorObject.unsignedShiftRight(AviatorTypeUtil.to(other), env));
    }

    @Override
    public ScriptObject bitNot(Map<String, Object> env) {
      return to(aviatorObject.bitNot(env));
    }

    @Override
    public ScriptObject sub(ScriptObject other, Map<String, Object> env) {
      return to(aviatorObject.sub(AviatorTypeUtil.to(other), env));
    }

    @Override
    public ScriptObject mod(ScriptObject other, Map<String, Object> env) {
      return to(aviatorObject.mod(AviatorTypeUtil.to(other), env));
    }

    @Override
    public ScriptObject div(ScriptObject other, Map<String, Object> env) {
      return to(aviatorObject.div(AviatorTypeUtil.to(other), env));
    }

    @Override
    public ScriptObject mult(ScriptObject other, Map<String, Object> env) {
      return to(aviatorObject.mult(AviatorTypeUtil.to(other), env));
    }

    @Override
    public Number numberValue(Map<String, Object> env) {
      return aviatorObject.numberValue(env);
    }

    @Override
    public String stringValue(Map<String, Object> env) {
      return aviatorObject.stringValue(env);
    }

    @Override
    public boolean booleanValue(Map<String, Object> env) {
      return aviatorObject.booleanValue(env);
    }

    @Override
    public ScriptObject getElement(Map<String, Object> env, ScriptObject indexObject) {
      return to(aviatorObject.getElement(env, AviatorTypeUtil.to(indexObject)));
    }
  }

  private static class ScriptAviatorFunction implements ScriptFunction, AviatorFunction {
    public static ScriptAviatorFunction to(ScriptFunction expressionFunction) {
      return new ScriptAviatorFunction(expressionFunction);
    }

    private final ScriptFunction scriptFunction;
    
    private ScriptAviatorFunction(ScriptFunction expressionFunction) {
      super();
      this.scriptFunction = expressionFunction;
    }

    @Override
    public String getName() {
      return scriptFunction.getName();
    }

    @Override
    public ScriptObject call(Map<String, Object> env, ScriptObject... args) {
      return scriptFunction.call(env, args);
    }

    @Override
    public AviatorObject call(Map<String, Object> env) {
      return AviatorTypeUtil.to(scriptFunction.call(env));
    }

    @Override
    public AviatorObject call(Map<String, Object> env, AviatorObject arg1) {
      return AviatorTypeUtil.to(scriptFunction.call(env, AviatorTypeUtil.to(arg1)));
    }

    @Override
    public AviatorObject call(Map<String, Object> env, AviatorObject arg1, AviatorObject arg2) {
      return AviatorTypeUtil
          .to(scriptFunction.call(env, AviatorTypeUtil.to(arg1), AviatorTypeUtil.to(arg2)));
    }

    @Override
    public AviatorObject call(Map<String, Object> env, AviatorObject arg1, AviatorObject arg2,
        AviatorObject arg3) {
      return AviatorTypeUtil.to(scriptFunction.call(env, AviatorTypeUtil.to(arg1),
          AviatorTypeUtil.to(arg2), AviatorTypeUtil.to(arg3)));
    }

    @Override
    public AviatorObject call(Map<String, Object> env, AviatorObject arg1, AviatorObject arg2,
        AviatorObject arg3, AviatorObject arg4) {
      return AviatorTypeUtil.to(scriptFunction.call(env, AviatorTypeUtil.to(arg1),
          AviatorTypeUtil.to(arg2), AviatorTypeUtil.to(arg3), AviatorTypeUtil.to(arg4)));
    }

    @Override
    public AviatorObject call(Map<String, Object> env, AviatorObject arg1, AviatorObject arg2,
        AviatorObject arg3, AviatorObject arg4, AviatorObject arg5) {
      return AviatorTypeUtil
          .to(scriptFunction.call(env, AviatorTypeUtil.to(arg1), AviatorTypeUtil.to(arg2),
              AviatorTypeUtil.to(arg3), AviatorTypeUtil.to(arg4), AviatorTypeUtil.to(arg5)));
    }

    @Override
    public AviatorObject call(Map<String, Object> env, AviatorObject arg1, AviatorObject arg2,
        AviatorObject arg3, AviatorObject arg4, AviatorObject arg5, AviatorObject arg6) {
      return AviatorTypeUtil.to(scriptFunction.call(env, AviatorTypeUtil.to(arg1),
          AviatorTypeUtil.to(arg2), AviatorTypeUtil.to(arg3), AviatorTypeUtil.to(arg4),
          AviatorTypeUtil.to(arg5), AviatorTypeUtil.to(arg6)));
    }

    @Override
    public AviatorObject call(Map<String, Object> env, AviatorObject arg1, AviatorObject arg2,
        AviatorObject arg3, AviatorObject arg4, AviatorObject arg5, AviatorObject arg6,
        AviatorObject arg7) {
      return AviatorTypeUtil.to(scriptFunction.call(env, AviatorTypeUtil.to(arg1),
          AviatorTypeUtil.to(arg2), AviatorTypeUtil.to(arg3), AviatorTypeUtil.to(arg4),
          AviatorTypeUtil.to(arg5), AviatorTypeUtil.to(arg6), AviatorTypeUtil.to(arg7)));
    }

    @Override
    public AviatorObject call(Map<String, Object> env, AviatorObject arg1, AviatorObject arg2,
        AviatorObject arg3, AviatorObject arg4, AviatorObject arg5, AviatorObject arg6,
        AviatorObject arg7, AviatorObject arg8) {
      return AviatorTypeUtil
          .to(scriptFunction.call(env, AviatorTypeUtil.to(arg1), AviatorTypeUtil.to(arg2),
              AviatorTypeUtil.to(arg3), AviatorTypeUtil.to(arg4), AviatorTypeUtil.to(arg5),
              AviatorTypeUtil.to(arg6), AviatorTypeUtil.to(arg7), AviatorTypeUtil.to(arg8)));
    }

    @Override
    public AviatorObject call(Map<String, Object> env, AviatorObject arg1, AviatorObject arg2,
        AviatorObject arg3, AviatorObject arg4, AviatorObject arg5, AviatorObject arg6,
        AviatorObject arg7, AviatorObject arg8, AviatorObject arg9) {
      return AviatorTypeUtil.to(scriptFunction.call(env, AviatorTypeUtil.to(arg1),
          AviatorTypeUtil.to(arg2), AviatorTypeUtil.to(arg3), AviatorTypeUtil.to(arg4),
          AviatorTypeUtil.to(arg5), AviatorTypeUtil.to(arg6), AviatorTypeUtil.to(arg7),
          AviatorTypeUtil.to(arg8), AviatorTypeUtil.to(arg9)));
    }

    @Override
    public AviatorObject call(Map<String, Object> env, AviatorObject arg1, AviatorObject arg2,
        AviatorObject arg3, AviatorObject arg4, AviatorObject arg5, AviatorObject arg6,
        AviatorObject arg7, AviatorObject arg8, AviatorObject arg9, AviatorObject arg10) {
      return AviatorTypeUtil.to(scriptFunction.call(env, AviatorTypeUtil.to(arg1),
          AviatorTypeUtil.to(arg2), AviatorTypeUtil.to(arg3), AviatorTypeUtil.to(arg4),
          AviatorTypeUtil.to(arg5), AviatorTypeUtil.to(arg6), AviatorTypeUtil.to(arg7),
          AviatorTypeUtil.to(arg8), AviatorTypeUtil.to(arg9), AviatorTypeUtil.to(arg10)));
    }

    @Override
    public AviatorObject call(Map<String, Object> env, AviatorObject arg1, AviatorObject arg2,
        AviatorObject arg3, AviatorObject arg4, AviatorObject arg5, AviatorObject arg6,
        AviatorObject arg7, AviatorObject arg8, AviatorObject arg9, AviatorObject arg10,
        AviatorObject arg11) {
      return AviatorTypeUtil
          .to(scriptFunction.call(env, AviatorTypeUtil.to(arg1), AviatorTypeUtil.to(arg2),
              AviatorTypeUtil.to(arg3), AviatorTypeUtil.to(arg4), AviatorTypeUtil.to(arg5),
              AviatorTypeUtil.to(arg6), AviatorTypeUtil.to(arg7), AviatorTypeUtil.to(arg8),
              AviatorTypeUtil.to(arg9), AviatorTypeUtil.to(arg10), AviatorTypeUtil.to(arg11)));
    }

    @Override
    public AviatorObject call(Map<String, Object> env, AviatorObject arg1, AviatorObject arg2,
        AviatorObject arg3, AviatorObject arg4, AviatorObject arg5, AviatorObject arg6,
        AviatorObject arg7, AviatorObject arg8, AviatorObject arg9, AviatorObject arg10,
        AviatorObject arg11, AviatorObject arg12) {
      return AviatorTypeUtil.to(scriptFunction.call(env, AviatorTypeUtil.to(arg1),
          AviatorTypeUtil.to(arg2), AviatorTypeUtil.to(arg3), AviatorTypeUtil.to(arg4),
          AviatorTypeUtil.to(arg5), AviatorTypeUtil.to(arg6), AviatorTypeUtil.to(arg7),
          AviatorTypeUtil.to(arg8), AviatorTypeUtil.to(arg9), AviatorTypeUtil.to(arg10),
          AviatorTypeUtil.to(arg11), AviatorTypeUtil.to(arg12)));
    }

    @Override
    public AviatorObject call(Map<String, Object> env, AviatorObject arg1, AviatorObject arg2,
        AviatorObject arg3, AviatorObject arg4, AviatorObject arg5, AviatorObject arg6,
        AviatorObject arg7, AviatorObject arg8, AviatorObject arg9, AviatorObject arg10,
        AviatorObject arg11, AviatorObject arg12, AviatorObject arg13) {
      return AviatorTypeUtil.to(scriptFunction.call(env, AviatorTypeUtil.to(arg1),
          AviatorTypeUtil.to(arg2), AviatorTypeUtil.to(arg3), AviatorTypeUtil.to(arg4),
          AviatorTypeUtil.to(arg5), AviatorTypeUtil.to(arg6), AviatorTypeUtil.to(arg7),
          AviatorTypeUtil.to(arg8), AviatorTypeUtil.to(arg9), AviatorTypeUtil.to(arg10),
          AviatorTypeUtil.to(arg11), AviatorTypeUtil.to(arg12), AviatorTypeUtil.to(arg13)));
    }

    @Override
    public AviatorObject call(Map<String, Object> env, AviatorObject arg1, AviatorObject arg2,
        AviatorObject arg3, AviatorObject arg4, AviatorObject arg5, AviatorObject arg6,
        AviatorObject arg7, AviatorObject arg8, AviatorObject arg9, AviatorObject arg10,
        AviatorObject arg11, AviatorObject arg12, AviatorObject arg13, AviatorObject arg14) {
      return AviatorTypeUtil
          .to(scriptFunction.call(env, AviatorTypeUtil.to(arg1), AviatorTypeUtil.to(arg2),
              AviatorTypeUtil.to(arg3), AviatorTypeUtil.to(arg4), AviatorTypeUtil.to(arg5),
              AviatorTypeUtil.to(arg6), AviatorTypeUtil.to(arg7), AviatorTypeUtil.to(arg8),
              AviatorTypeUtil.to(arg9), AviatorTypeUtil.to(arg10), AviatorTypeUtil.to(arg11),
              AviatorTypeUtil.to(arg12), AviatorTypeUtil.to(arg13), AviatorTypeUtil.to(arg14)));
    }

    @Override
    public AviatorObject call(Map<String, Object> env, AviatorObject arg1, AviatorObject arg2,
        AviatorObject arg3, AviatorObject arg4, AviatorObject arg5, AviatorObject arg6,
        AviatorObject arg7, AviatorObject arg8, AviatorObject arg9, AviatorObject arg10,
        AviatorObject arg11, AviatorObject arg12, AviatorObject arg13, AviatorObject arg14,
        AviatorObject arg15) {
      return AviatorTypeUtil.to(scriptFunction.call(env, AviatorTypeUtil.to(arg1),
          AviatorTypeUtil.to(arg2), AviatorTypeUtil.to(arg3), AviatorTypeUtil.to(arg4),
          AviatorTypeUtil.to(arg5), AviatorTypeUtil.to(arg6), AviatorTypeUtil.to(arg7),
          AviatorTypeUtil.to(arg8), AviatorTypeUtil.to(arg9), AviatorTypeUtil.to(arg10),
          AviatorTypeUtil.to(arg11), AviatorTypeUtil.to(arg12), AviatorTypeUtil.to(arg13),
          AviatorTypeUtil.to(arg14), AviatorTypeUtil.to(arg15)));
    }

    @Override
    public AviatorObject call(Map<String, Object> env, AviatorObject arg1, AviatorObject arg2,
        AviatorObject arg3, AviatorObject arg4, AviatorObject arg5, AviatorObject arg6,
        AviatorObject arg7, AviatorObject arg8, AviatorObject arg9, AviatorObject arg10,
        AviatorObject arg11, AviatorObject arg12, AviatorObject arg13, AviatorObject arg14,
        AviatorObject arg15, AviatorObject arg16) {
      return AviatorTypeUtil.to(scriptFunction.call(env, AviatorTypeUtil.to(arg1),
          AviatorTypeUtil.to(arg2), AviatorTypeUtil.to(arg3), AviatorTypeUtil.to(arg4),
          AviatorTypeUtil.to(arg5), AviatorTypeUtil.to(arg6), AviatorTypeUtil.to(arg7),
          AviatorTypeUtil.to(arg8), AviatorTypeUtil.to(arg9), AviatorTypeUtil.to(arg10),
          AviatorTypeUtil.to(arg11), AviatorTypeUtil.to(arg12), AviatorTypeUtil.to(arg13),
          AviatorTypeUtil.to(arg14), AviatorTypeUtil.to(arg15), AviatorTypeUtil.to(arg16)));
    }

    @Override
    public AviatorObject call(Map<String, Object> env, AviatorObject arg1, AviatorObject arg2,
        AviatorObject arg3, AviatorObject arg4, AviatorObject arg5, AviatorObject arg6,
        AviatorObject arg7, AviatorObject arg8, AviatorObject arg9, AviatorObject arg10,
        AviatorObject arg11, AviatorObject arg12, AviatorObject arg13, AviatorObject arg14,
        AviatorObject arg15, AviatorObject arg16, AviatorObject arg17) {
      return AviatorTypeUtil
          .to(scriptFunction.call(env, AviatorTypeUtil.to(arg1), AviatorTypeUtil.to(arg2),
              AviatorTypeUtil.to(arg3), AviatorTypeUtil.to(arg4), AviatorTypeUtil.to(arg5),
              AviatorTypeUtil.to(arg6), AviatorTypeUtil.to(arg7), AviatorTypeUtil.to(arg8),
              AviatorTypeUtil.to(arg9), AviatorTypeUtil.to(arg10), AviatorTypeUtil.to(arg11),
              AviatorTypeUtil.to(arg12), AviatorTypeUtil.to(arg13), AviatorTypeUtil.to(arg14),
              AviatorTypeUtil.to(arg15), AviatorTypeUtil.to(arg16), AviatorTypeUtil.to(arg17)));
    }

    @Override
    public AviatorObject call(Map<String, Object> env, AviatorObject arg1, AviatorObject arg2,
        AviatorObject arg3, AviatorObject arg4, AviatorObject arg5, AviatorObject arg6,
        AviatorObject arg7, AviatorObject arg8, AviatorObject arg9, AviatorObject arg10,
        AviatorObject arg11, AviatorObject arg12, AviatorObject arg13, AviatorObject arg14,
        AviatorObject arg15, AviatorObject arg16, AviatorObject arg17, AviatorObject arg18) {
      return AviatorTypeUtil.to(scriptFunction.call(env, AviatorTypeUtil.to(arg1),
          AviatorTypeUtil.to(arg2), AviatorTypeUtil.to(arg3), AviatorTypeUtil.to(arg4),
          AviatorTypeUtil.to(arg5), AviatorTypeUtil.to(arg6), AviatorTypeUtil.to(arg7),
          AviatorTypeUtil.to(arg8), AviatorTypeUtil.to(arg9), AviatorTypeUtil.to(arg10),
          AviatorTypeUtil.to(arg11), AviatorTypeUtil.to(arg12), AviatorTypeUtil.to(arg13),
          AviatorTypeUtil.to(arg14), AviatorTypeUtil.to(arg15), AviatorTypeUtil.to(arg16),
          AviatorTypeUtil.to(arg17), AviatorTypeUtil.to(arg18)));
    }

    @Override
    public AviatorObject call(Map<String, Object> env, AviatorObject arg1, AviatorObject arg2,
        AviatorObject arg3, AviatorObject arg4, AviatorObject arg5, AviatorObject arg6,
        AviatorObject arg7, AviatorObject arg8, AviatorObject arg9, AviatorObject arg10,
        AviatorObject arg11, AviatorObject arg12, AviatorObject arg13, AviatorObject arg14,
        AviatorObject arg15, AviatorObject arg16, AviatorObject arg17, AviatorObject arg18,
        AviatorObject arg19) {
      return AviatorTypeUtil.to(scriptFunction.call(env, AviatorTypeUtil.to(arg1),
          AviatorTypeUtil.to(arg2), AviatorTypeUtil.to(arg3), AviatorTypeUtil.to(arg4),
          AviatorTypeUtil.to(arg5), AviatorTypeUtil.to(arg6), AviatorTypeUtil.to(arg7),
          AviatorTypeUtil.to(arg8), AviatorTypeUtil.to(arg9), AviatorTypeUtil.to(arg10),
          AviatorTypeUtil.to(arg11), AviatorTypeUtil.to(arg12), AviatorTypeUtil.to(arg13),
          AviatorTypeUtil.to(arg14), AviatorTypeUtil.to(arg15), AviatorTypeUtil.to(arg16),
          AviatorTypeUtil.to(arg17), AviatorTypeUtil.to(arg18), AviatorTypeUtil.to(arg19)));
    }

    @Override
    public AviatorObject call(Map<String, Object> env, AviatorObject arg1, AviatorObject arg2,
        AviatorObject arg3, AviatorObject arg4, AviatorObject arg5, AviatorObject arg6,
        AviatorObject arg7, AviatorObject arg8, AviatorObject arg9, AviatorObject arg10,
        AviatorObject arg11, AviatorObject arg12, AviatorObject arg13, AviatorObject arg14,
        AviatorObject arg15, AviatorObject arg16, AviatorObject arg17, AviatorObject arg18,
        AviatorObject arg19, AviatorObject arg20) {
      return AviatorTypeUtil
          .to(scriptFunction.call(env, AviatorTypeUtil.to(arg1), AviatorTypeUtil.to(arg2),
              AviatorTypeUtil.to(arg3), AviatorTypeUtil.to(arg4), AviatorTypeUtil.to(arg5),
              AviatorTypeUtil.to(arg6), AviatorTypeUtil.to(arg7), AviatorTypeUtil.to(arg8),
              AviatorTypeUtil.to(arg9), AviatorTypeUtil.to(arg10), AviatorTypeUtil.to(arg11),
              AviatorTypeUtil.to(arg12), AviatorTypeUtil.to(arg13), AviatorTypeUtil.to(arg14),
              AviatorTypeUtil.to(arg15), AviatorTypeUtil.to(arg16), AviatorTypeUtil.to(arg17),
              AviatorTypeUtil.to(arg18), AviatorTypeUtil.to(arg19), AviatorTypeUtil.to(arg20)));
    }

    @Override
    public AviatorObject call(Map<String, Object> env, AviatorObject arg1, AviatorObject arg2,
        AviatorObject arg3, AviatorObject arg4, AviatorObject arg5, AviatorObject arg6,
        AviatorObject arg7, AviatorObject arg8, AviatorObject arg9, AviatorObject arg10,
        AviatorObject arg11, AviatorObject arg12, AviatorObject arg13, AviatorObject arg14,
        AviatorObject arg15, AviatorObject arg16, AviatorObject arg17, AviatorObject arg18,
        AviatorObject arg19, AviatorObject arg20, AviatorObject... args) {
      if (args == null || args.length == 0) {
        return call(env, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12,
            arg13, arg14, arg15, arg16, arg17, arg18, arg19, arg20);
      } else {
        ScriptObject[] allArgs = new ScriptObject[20 + args.length];
        ScriptObject[] firstTwentyArgs =
            AviatorTypeUtil.to(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11,
                arg12, arg13, arg14, arg15, arg16, arg17, arg18, arg19, arg20);
        ScriptObject[] otherArgs = AviatorTypeUtil.to(args);
        System.arraycopy(firstTwentyArgs, 0, allArgs, 0, firstTwentyArgs.length);
        System.arraycopy(otherArgs, 0, allArgs, firstTwentyArgs.length, otherArgs.length);
        return AviatorTypeUtil.to(scriptFunction.call(env, allArgs));
      }
    }

  }

  private static class AviatorScriptFunction implements ScriptFunction, AviatorFunction {
    public static AviatorScriptFunction to(AviatorFunction aviatorFunction) {
      return new AviatorScriptFunction(aviatorFunction);
    }

    private final AviatorFunction aviatorFunction;

    private AviatorScriptFunction(AviatorFunction aviatorFunction) {
      this.aviatorFunction = aviatorFunction;
    }

    @Override
    public String getName() {
      return aviatorFunction.getName();
    }

    @Override
    public ScriptObject call(Map<String, Object> env, ScriptObject... args) {
      AviatorObject aviatorObject = null;
      switch (args.length) {
        case 0:
          aviatorObject = aviatorFunction.call(env);
          break;
        case 1:
          aviatorObject = aviatorFunction.call(env, AviatorTypeUtil.to(args[0]));
          break;
        case 2:
          aviatorObject =
              aviatorFunction.call(env, AviatorTypeUtil.to(args[0]), AviatorTypeUtil.to(args[1]));
          break;
        case 3:
          aviatorObject = aviatorFunction.call(env, AviatorTypeUtil.to(args[0]),
              AviatorTypeUtil.to(args[1]), AviatorTypeUtil.to(args[2]));
          break;
        case 4:
          aviatorObject =
              aviatorFunction.call(env, AviatorTypeUtil.to(args[0]), AviatorTypeUtil.to(args[1]),
                  AviatorTypeUtil.to(args[2]), AviatorTypeUtil.to(args[3]));
          break;
        case 5:
          aviatorObject = aviatorFunction.call(env, AviatorTypeUtil.to(args[0]),
              AviatorTypeUtil.to(args[1]), AviatorTypeUtil.to(args[2]), AviatorTypeUtil.to(args[3]),
              AviatorTypeUtil.to(args[4]));
          break;
        case 6:
          aviatorObject = aviatorFunction.call(env, AviatorTypeUtil.to(args[0]),
              AviatorTypeUtil.to(args[1]), AviatorTypeUtil.to(args[2]), AviatorTypeUtil.to(args[3]),
              AviatorTypeUtil.to(args[4]), AviatorTypeUtil.to(args[5]));
          break;
        case 7:
          aviatorObject = aviatorFunction.call(env, AviatorTypeUtil.to(args[0]),
              AviatorTypeUtil.to(args[1]), AviatorTypeUtil.to(args[2]), AviatorTypeUtil.to(args[3]),
              AviatorTypeUtil.to(args[4]), AviatorTypeUtil.to(args[5]),
              AviatorTypeUtil.to(args[6]));
          break;
        case 8:
          aviatorObject = aviatorFunction.call(env, AviatorTypeUtil.to(args[0]),
              AviatorTypeUtil.to(args[1]), AviatorTypeUtil.to(args[2]), AviatorTypeUtil.to(args[3]),
              AviatorTypeUtil.to(args[4]), AviatorTypeUtil.to(args[5]), AviatorTypeUtil.to(args[6]),
              AviatorTypeUtil.to(args[7]));
          break;
        case 9:
          aviatorObject = aviatorFunction.call(env, AviatorTypeUtil.to(args[0]),
              AviatorTypeUtil.to(args[1]), AviatorTypeUtil.to(args[2]), AviatorTypeUtil.to(args[3]),
              AviatorTypeUtil.to(args[4]), AviatorTypeUtil.to(args[5]), AviatorTypeUtil.to(args[6]),
              AviatorTypeUtil.to(args[7]), AviatorTypeUtil.to(args[8]));
          break;
        case 10:
          aviatorObject = aviatorFunction.call(env, AviatorTypeUtil.to(args[0]),
              AviatorTypeUtil.to(args[1]), AviatorTypeUtil.to(args[2]), AviatorTypeUtil.to(args[3]),
              AviatorTypeUtil.to(args[4]), AviatorTypeUtil.to(args[5]), AviatorTypeUtil.to(args[6]),
              AviatorTypeUtil.to(args[7]), AviatorTypeUtil.to(args[8]),
              AviatorTypeUtil.to(args[9]));
          break;
        case 11:
          aviatorObject = aviatorFunction.call(env, AviatorTypeUtil.to(args[0]),
              AviatorTypeUtil.to(args[1]), AviatorTypeUtil.to(args[2]), AviatorTypeUtil.to(args[3]),
              AviatorTypeUtil.to(args[4]), AviatorTypeUtil.to(args[5]), AviatorTypeUtil.to(args[6]),
              AviatorTypeUtil.to(args[7]), AviatorTypeUtil.to(args[8]), AviatorTypeUtil.to(args[9]),
              AviatorTypeUtil.to(args[10]));
          break;
        case 12:
          aviatorObject = aviatorFunction.call(env, AviatorTypeUtil.to(args[0]),
              AviatorTypeUtil.to(args[1]), AviatorTypeUtil.to(args[2]), AviatorTypeUtil.to(args[3]),
              AviatorTypeUtil.to(args[4]), AviatorTypeUtil.to(args[5]), AviatorTypeUtil.to(args[6]),
              AviatorTypeUtil.to(args[7]), AviatorTypeUtil.to(args[8]), AviatorTypeUtil.to(args[9]),
              AviatorTypeUtil.to(args[10]), AviatorTypeUtil.to(args[11]));
          break;
        case 13:
          aviatorObject = aviatorFunction.call(env, AviatorTypeUtil.to(args[0]),
              AviatorTypeUtil.to(args[1]), AviatorTypeUtil.to(args[2]), AviatorTypeUtil.to(args[3]),
              AviatorTypeUtil.to(args[4]), AviatorTypeUtil.to(args[5]), AviatorTypeUtil.to(args[6]),
              AviatorTypeUtil.to(args[7]), AviatorTypeUtil.to(args[8]), AviatorTypeUtil.to(args[9]),
              AviatorTypeUtil.to(args[10]), AviatorTypeUtil.to(args[11]),
              AviatorTypeUtil.to(args[12]));
          break;
        case 14:
          aviatorObject = aviatorFunction.call(env, AviatorTypeUtil.to(args[0]),
              AviatorTypeUtil.to(args[1]), AviatorTypeUtil.to(args[2]), AviatorTypeUtil.to(args[3]),
              AviatorTypeUtil.to(args[4]), AviatorTypeUtil.to(args[5]), AviatorTypeUtil.to(args[6]),
              AviatorTypeUtil.to(args[7]), AviatorTypeUtil.to(args[8]), AviatorTypeUtil.to(args[9]),
              AviatorTypeUtil.to(args[10]), AviatorTypeUtil.to(args[11]),
              AviatorTypeUtil.to(args[12]), AviatorTypeUtil.to(args[13]));
          break;
        case 15:
          aviatorObject = aviatorFunction.call(env, AviatorTypeUtil.to(args[0]),
              AviatorTypeUtil.to(args[1]), AviatorTypeUtil.to(args[2]), AviatorTypeUtil.to(args[3]),
              AviatorTypeUtil.to(args[4]), AviatorTypeUtil.to(args[5]), AviatorTypeUtil.to(args[6]),
              AviatorTypeUtil.to(args[7]), AviatorTypeUtil.to(args[8]), AviatorTypeUtil.to(args[9]),
              AviatorTypeUtil.to(args[10]), AviatorTypeUtil.to(args[11]),
              AviatorTypeUtil.to(args[12]), AviatorTypeUtil.to(args[13]),
              AviatorTypeUtil.to(args[14]));
          break;
        case 16:
          aviatorObject = aviatorFunction.call(env, AviatorTypeUtil.to(args[0]),
              AviatorTypeUtil.to(args[1]), AviatorTypeUtil.to(args[2]), AviatorTypeUtil.to(args[3]),
              AviatorTypeUtil.to(args[4]), AviatorTypeUtil.to(args[5]), AviatorTypeUtil.to(args[6]),
              AviatorTypeUtil.to(args[7]), AviatorTypeUtil.to(args[8]), AviatorTypeUtil.to(args[9]),
              AviatorTypeUtil.to(args[10]), AviatorTypeUtil.to(args[11]),
              AviatorTypeUtil.to(args[12]), AviatorTypeUtil.to(args[13]),
              AviatorTypeUtil.to(args[14]), AviatorTypeUtil.to(args[15]));
          break;
        case 17:
          aviatorObject = aviatorFunction.call(env, AviatorTypeUtil.to(args[0]),
              AviatorTypeUtil.to(args[1]), AviatorTypeUtil.to(args[2]), AviatorTypeUtil.to(args[3]),
              AviatorTypeUtil.to(args[4]), AviatorTypeUtil.to(args[5]), AviatorTypeUtil.to(args[6]),
              AviatorTypeUtil.to(args[7]), AviatorTypeUtil.to(args[8]), AviatorTypeUtil.to(args[9]),
              AviatorTypeUtil.to(args[10]), AviatorTypeUtil.to(args[11]),
              AviatorTypeUtil.to(args[12]), AviatorTypeUtil.to(args[13]),
              AviatorTypeUtil.to(args[14]), AviatorTypeUtil.to(args[15]),
              AviatorTypeUtil.to(args[16]));
          break;
        case 18:
          aviatorObject = aviatorFunction.call(env, AviatorTypeUtil.to(args[0]),
              AviatorTypeUtil.to(args[1]), AviatorTypeUtil.to(args[2]), AviatorTypeUtil.to(args[3]),
              AviatorTypeUtil.to(args[4]), AviatorTypeUtil.to(args[5]), AviatorTypeUtil.to(args[6]),
              AviatorTypeUtil.to(args[7]), AviatorTypeUtil.to(args[8]), AviatorTypeUtil.to(args[9]),
              AviatorTypeUtil.to(args[10]), AviatorTypeUtil.to(args[11]),
              AviatorTypeUtil.to(args[12]), AviatorTypeUtil.to(args[13]),
              AviatorTypeUtil.to(args[14]), AviatorTypeUtil.to(args[15]),
              AviatorTypeUtil.to(args[16]), AviatorTypeUtil.to(args[17]));
          break;
        case 19:
          aviatorObject = aviatorFunction.call(env, AviatorTypeUtil.to(args[0]),
              AviatorTypeUtil.to(args[1]), AviatorTypeUtil.to(args[2]), AviatorTypeUtil.to(args[3]),
              AviatorTypeUtil.to(args[4]), AviatorTypeUtil.to(args[5]), AviatorTypeUtil.to(args[6]),
              AviatorTypeUtil.to(args[7]), AviatorTypeUtil.to(args[8]), AviatorTypeUtil.to(args[9]),
              AviatorTypeUtil.to(args[10]), AviatorTypeUtil.to(args[11]),
              AviatorTypeUtil.to(args[12]), AviatorTypeUtil.to(args[13]),
              AviatorTypeUtil.to(args[14]), AviatorTypeUtil.to(args[15]),
              AviatorTypeUtil.to(args[16]), AviatorTypeUtil.to(args[17]),
              AviatorTypeUtil.to(args[18]));
          break;
        case 20:
          aviatorObject = aviatorFunction.call(env, AviatorTypeUtil.to(args[0]),
              AviatorTypeUtil.to(args[1]), AviatorTypeUtil.to(args[2]), AviatorTypeUtil.to(args[3]),
              AviatorTypeUtil.to(args[4]), AviatorTypeUtil.to(args[5]), AviatorTypeUtil.to(args[6]),
              AviatorTypeUtil.to(args[7]), AviatorTypeUtil.to(args[8]), AviatorTypeUtil.to(args[9]),
              AviatorTypeUtil.to(args[10]), AviatorTypeUtil.to(args[11]),
              AviatorTypeUtil.to(args[12]), AviatorTypeUtil.to(args[13]),
              AviatorTypeUtil.to(args[14]), AviatorTypeUtil.to(args[15]),
              AviatorTypeUtil.to(args[16]), AviatorTypeUtil.to(args[17]),
              AviatorTypeUtil.to(args[18]), AviatorTypeUtil.to(args[19]));
          break;
        default:
          AviatorObject[] allArgs = AviatorTypeUtil.to(args);
          AviatorObject[] variableArgs = new AviatorObject[allArgs.length - 20];
          System.arraycopy(allArgs, 20, variableArgs, 0, variableArgs.length);
          aviatorObject = aviatorFunction.call(env, allArgs[0], allArgs[1], allArgs[2], allArgs[3],
              allArgs[4], allArgs[5], allArgs[6], allArgs[7], allArgs[8], allArgs[9], allArgs[10],
              allArgs[11], allArgs[12], allArgs[13], allArgs[14], allArgs[15], allArgs[16],
              allArgs[17], allArgs[18], allArgs[19], variableArgs);
      }
      return AviatorTypeUtil.to(aviatorObject);
    }

    @Override
    public AviatorObject call(Map<String, Object> env) {
      return aviatorFunction.call(env);
    }

    @Override
    public AviatorObject call(Map<String, Object> env, AviatorObject arg1) {
      return aviatorFunction.call(env, arg1);
    }

    @Override
    public AviatorObject call(Map<String, Object> env, AviatorObject arg1, AviatorObject arg2) {
      return aviatorFunction.call(env, arg1, arg2);
    }

    @Override
    public AviatorObject call(Map<String, Object> env, AviatorObject arg1, AviatorObject arg2,
        AviatorObject arg3) {
      return aviatorFunction.call(env, arg1, arg2, arg3);
    }

    @Override
    public AviatorObject call(Map<String, Object> env, AviatorObject arg1, AviatorObject arg2,
        AviatorObject arg3, AviatorObject arg4) {
      return aviatorFunction.call(env, arg1, arg2, arg3, arg4);
    }

    @Override
    public AviatorObject call(Map<String, Object> env, AviatorObject arg1, AviatorObject arg2,
        AviatorObject arg3, AviatorObject arg4, AviatorObject arg5) {
      return aviatorFunction.call(env, arg1, arg2, arg3, arg4, arg5);
    }

    @Override
    public AviatorObject call(Map<String, Object> env, AviatorObject arg1, AviatorObject arg2,
        AviatorObject arg3, AviatorObject arg4, AviatorObject arg5, AviatorObject arg6) {
      return aviatorFunction.call(env, arg1, arg2, arg3, arg4, arg5, arg6);
    }

    @Override
    public AviatorObject call(Map<String, Object> env, AviatorObject arg1, AviatorObject arg2,
        AviatorObject arg3, AviatorObject arg4, AviatorObject arg5, AviatorObject arg6,
        AviatorObject arg7) {
      return aviatorFunction.call(env, arg1, arg2, arg3, arg4, arg5, arg6, arg7);
    }

    @Override
    public AviatorObject call(Map<String, Object> env, AviatorObject arg1, AviatorObject arg2,
        AviatorObject arg3, AviatorObject arg4, AviatorObject arg5, AviatorObject arg6,
        AviatorObject arg7, AviatorObject arg8) {
      return aviatorFunction.call(env, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8);
    }

    @Override
    public AviatorObject call(Map<String, Object> env, AviatorObject arg1, AviatorObject arg2,
        AviatorObject arg3, AviatorObject arg4, AviatorObject arg5, AviatorObject arg6,
        AviatorObject arg7, AviatorObject arg8, AviatorObject arg9) {
      return aviatorFunction.call(env, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9);
    }

    @Override
    public AviatorObject call(Map<String, Object> env, AviatorObject arg1, AviatorObject arg2,
        AviatorObject arg3, AviatorObject arg4, AviatorObject arg5, AviatorObject arg6,
        AviatorObject arg7, AviatorObject arg8, AviatorObject arg9, AviatorObject arg10) {
      return aviatorFunction.call(env, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10);
    }

    @Override
    public AviatorObject call(Map<String, Object> env, AviatorObject arg1, AviatorObject arg2,
        AviatorObject arg3, AviatorObject arg4, AviatorObject arg5, AviatorObject arg6,
        AviatorObject arg7, AviatorObject arg8, AviatorObject arg9, AviatorObject arg10,
        AviatorObject arg11) {
      return aviatorFunction.call(env, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10,
          arg11);
    }

    @Override
    public AviatorObject call(Map<String, Object> env, AviatorObject arg1, AviatorObject arg2,
        AviatorObject arg3, AviatorObject arg4, AviatorObject arg5, AviatorObject arg6,
        AviatorObject arg7, AviatorObject arg8, AviatorObject arg9, AviatorObject arg10,
        AviatorObject arg11, AviatorObject arg12) {
      return aviatorFunction.call(env, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10,
          arg11, arg12);
    }

    @Override
    public AviatorObject call(Map<String, Object> env, AviatorObject arg1, AviatorObject arg2,
        AviatorObject arg3, AviatorObject arg4, AviatorObject arg5, AviatorObject arg6,
        AviatorObject arg7, AviatorObject arg8, AviatorObject arg9, AviatorObject arg10,
        AviatorObject arg11, AviatorObject arg12, AviatorObject arg13) {
      return aviatorFunction.call(env, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10,
          arg11, arg12, arg13);
    }

    @Override
    public AviatorObject call(Map<String, Object> env, AviatorObject arg1, AviatorObject arg2,
        AviatorObject arg3, AviatorObject arg4, AviatorObject arg5, AviatorObject arg6,
        AviatorObject arg7, AviatorObject arg8, AviatorObject arg9, AviatorObject arg10,
        AviatorObject arg11, AviatorObject arg12, AviatorObject arg13, AviatorObject arg14) {
      return aviatorFunction.call(env, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10,
          arg11, arg12, arg13, arg14);
    }

    @Override
    public AviatorObject call(Map<String, Object> env, AviatorObject arg1, AviatorObject arg2,
        AviatorObject arg3, AviatorObject arg4, AviatorObject arg5, AviatorObject arg6,
        AviatorObject arg7, AviatorObject arg8, AviatorObject arg9, AviatorObject arg10,
        AviatorObject arg11, AviatorObject arg12, AviatorObject arg13, AviatorObject arg14,
        AviatorObject arg15) {
      return aviatorFunction.call(env, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10,
          arg11, arg12, arg13, arg14, arg15);
    }

    @Override
    public AviatorObject call(Map<String, Object> env, AviatorObject arg1, AviatorObject arg2,
        AviatorObject arg3, AviatorObject arg4, AviatorObject arg5, AviatorObject arg6,
        AviatorObject arg7, AviatorObject arg8, AviatorObject arg9, AviatorObject arg10,
        AviatorObject arg11, AviatorObject arg12, AviatorObject arg13, AviatorObject arg14,
        AviatorObject arg15, AviatorObject arg16) {
      return aviatorFunction.call(env, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10,
          arg11, arg12, arg13, arg14, arg15, arg16);
    }

    @Override
    public AviatorObject call(Map<String, Object> env, AviatorObject arg1, AviatorObject arg2,
        AviatorObject arg3, AviatorObject arg4, AviatorObject arg5, AviatorObject arg6,
        AviatorObject arg7, AviatorObject arg8, AviatorObject arg9, AviatorObject arg10,
        AviatorObject arg11, AviatorObject arg12, AviatorObject arg13, AviatorObject arg14,
        AviatorObject arg15, AviatorObject arg16, AviatorObject arg17) {
      return aviatorFunction.call(env, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10,
          arg11, arg12, arg13, arg14, arg15, arg16, arg17);
    }

    @Override
    public AviatorObject call(Map<String, Object> env, AviatorObject arg1, AviatorObject arg2,
        AviatorObject arg3, AviatorObject arg4, AviatorObject arg5, AviatorObject arg6,
        AviatorObject arg7, AviatorObject arg8, AviatorObject arg9, AviatorObject arg10,
        AviatorObject arg11, AviatorObject arg12, AviatorObject arg13, AviatorObject arg14,
        AviatorObject arg15, AviatorObject arg16, AviatorObject arg17, AviatorObject arg18) {
      return aviatorFunction.call(env, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10,
          arg11, arg12, arg13, arg14, arg15, arg16, arg17, arg18);
    }

    @Override
    public AviatorObject call(Map<String, Object> env, AviatorObject arg1, AviatorObject arg2,
        AviatorObject arg3, AviatorObject arg4, AviatorObject arg5, AviatorObject arg6,
        AviatorObject arg7, AviatorObject arg8, AviatorObject arg9, AviatorObject arg10,
        AviatorObject arg11, AviatorObject arg12, AviatorObject arg13, AviatorObject arg14,
        AviatorObject arg15, AviatorObject arg16, AviatorObject arg17, AviatorObject arg18,
        AviatorObject arg19) {
      return aviatorFunction.call(env, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10,
          arg11, arg12, arg13, arg14, arg15, arg16, arg17, arg18, arg19);
    }

    @Override
    public AviatorObject call(Map<String, Object> env, AviatorObject arg1, AviatorObject arg2,
        AviatorObject arg3, AviatorObject arg4, AviatorObject arg5, AviatorObject arg6,
        AviatorObject arg7, AviatorObject arg8, AviatorObject arg9, AviatorObject arg10,
        AviatorObject arg11, AviatorObject arg12, AviatorObject arg13, AviatorObject arg14,
        AviatorObject arg15, AviatorObject arg16, AviatorObject arg17, AviatorObject arg18,
        AviatorObject arg19, AviatorObject arg20) {
      return aviatorFunction.call(env, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10,
          arg11, arg12, arg13, arg14, arg15, arg16, arg17, arg18, arg19, arg20);
    }

    @Override
    public AviatorObject call(Map<String, Object> env, AviatorObject arg1, AviatorObject arg2,
        AviatorObject arg3, AviatorObject arg4, AviatorObject arg5, AviatorObject arg6,
        AviatorObject arg7, AviatorObject arg8, AviatorObject arg9, AviatorObject arg10,
        AviatorObject arg11, AviatorObject arg12, AviatorObject arg13, AviatorObject arg14,
        AviatorObject arg15, AviatorObject arg16, AviatorObject arg17, AviatorObject arg18,
        AviatorObject arg19, AviatorObject arg20, AviatorObject... args) {
      return aviatorFunction.call(env, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10,
          arg11, arg12, arg13, arg14, arg15, arg16, arg17, arg18, arg19, arg20, args);
    }
  }

  private static class AviatorScriptClassLoader extends ScriptClassLoader {
    public static AviatorScriptClassLoader to(AviatorClassLoader classLoader) {
      return new AviatorScriptClassLoader(classLoader);
    }

    @SuppressWarnings("unused")
    private final AviatorClassLoader classLoader;

    private AviatorScriptClassLoader(AviatorClassLoader classLoader) {
      super(classLoader.getParent());
      this.classLoader = classLoader;
    }
  }

  private static class AviatorScriptExpression implements Expression, ScriptExpression {
    public static AviatorScriptExpression to(Expression expression) {
      return new AviatorScriptExpression(expression);
    }

    private final Expression expression;

    private AviatorScriptExpression(Expression expression) {
      super();
      this.expression = expression;
    }

    @Override
    public Object execute(Map<String, Object> env) {
      return expression.execute(env);
    }

    @Override
    public Object execute() {
      return expression.execute();
    }

    @Override
    public List<String> getVariableNames() {
      return expression.getVariableNames();
    }

    @Override
    public List<String> getVariableFullNames() {
      return expression.getVariableFullNames();
    }

  }
}
