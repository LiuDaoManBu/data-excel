package com.caotc.excel4j.expression;

import java.util.Map;
import com.googlecode.aviator.runtime.type.AviatorFunction;
import com.googlecode.aviator.runtime.type.AviatorObject;

public class BaseExpressionFunction implements ExpressionFunction {
  public static BaseExpressionFunction valueOf(AviatorFunction aviatorFunction) {
    return new BaseExpressionFunction(aviatorFunction);
  }

  private AviatorFunction aviatorFunction;

  private BaseExpressionFunction(AviatorFunction aviatorFunction) {
    this.aviatorFunction = aviatorFunction;
  }

  public String getName() {
    return aviatorFunction.getName();
  }

  public AviatorObject call(Map<String, Object> env) {
    return aviatorFunction.call(env);
  }

  public AviatorObject call(Map<String, Object> env, AviatorObject arg1) {
    return aviatorFunction.call(env, arg1);
  }

  public AviatorObject call(Map<String, Object> env, AviatorObject arg1, AviatorObject arg2) {
    return aviatorFunction.call(env, arg1, arg2);
  }

  public AviatorObject call(Map<String, Object> env, AviatorObject arg1, AviatorObject arg2,
      AviatorObject arg3) {
    return aviatorFunction.call(env, arg1, arg2, arg3);
  }

  public AviatorObject call(Map<String, Object> env, AviatorObject arg1, AviatorObject arg2,
      AviatorObject arg3, AviatorObject arg4) {
    return aviatorFunction.call(env, arg1, arg2, arg3, arg4);
  }

  public AviatorObject call(Map<String, Object> env, AviatorObject arg1, AviatorObject arg2,
      AviatorObject arg3, AviatorObject arg4, AviatorObject arg5) {
    return aviatorFunction.call(env, arg1, arg2, arg3, arg4, arg5);
  }

  public AviatorObject call(Map<String, Object> env, AviatorObject arg1, AviatorObject arg2,
      AviatorObject arg3, AviatorObject arg4, AviatorObject arg5, AviatorObject arg6) {
    return aviatorFunction.call(env, arg1, arg2, arg3, arg4, arg5, arg6);
  }

  public AviatorObject call(Map<String, Object> env, AviatorObject arg1, AviatorObject arg2,
      AviatorObject arg3, AviatorObject arg4, AviatorObject arg5, AviatorObject arg6,
      AviatorObject arg7) {
    return aviatorFunction.call(env, arg1, arg2, arg3, arg4, arg5, arg6, arg7);
  }

  public AviatorObject call(Map<String, Object> env, AviatorObject arg1, AviatorObject arg2,
      AviatorObject arg3, AviatorObject arg4, AviatorObject arg5, AviatorObject arg6,
      AviatorObject arg7, AviatorObject arg8) {
    return aviatorFunction.call(env, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8);
  }

  public AviatorObject call(Map<String, Object> env, AviatorObject arg1, AviatorObject arg2,
      AviatorObject arg3, AviatorObject arg4, AviatorObject arg5, AviatorObject arg6,
      AviatorObject arg7, AviatorObject arg8, AviatorObject arg9) {
    return aviatorFunction.call(env, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9);
  }

  public AviatorObject call(Map<String, Object> env, AviatorObject arg1, AviatorObject arg2,
      AviatorObject arg3, AviatorObject arg4, AviatorObject arg5, AviatorObject arg6,
      AviatorObject arg7, AviatorObject arg8, AviatorObject arg9, AviatorObject arg10) {
    return aviatorFunction.call(env, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10);
  }

  public AviatorObject call(Map<String, Object> env, AviatorObject arg1, AviatorObject arg2,
      AviatorObject arg3, AviatorObject arg4, AviatorObject arg5, AviatorObject arg6,
      AviatorObject arg7, AviatorObject arg8, AviatorObject arg9, AviatorObject arg10,
      AviatorObject arg11) {
    return aviatorFunction.call(env, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10,
        arg11);
  }

  public AviatorObject call(Map<String, Object> env, AviatorObject arg1, AviatorObject arg2,
      AviatorObject arg3, AviatorObject arg4, AviatorObject arg5, AviatorObject arg6,
      AviatorObject arg7, AviatorObject arg8, AviatorObject arg9, AviatorObject arg10,
      AviatorObject arg11, AviatorObject arg12) {
    return aviatorFunction.call(env, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10,
        arg11, arg12);
  }

  public AviatorObject call(Map<String, Object> env, AviatorObject arg1, AviatorObject arg2,
      AviatorObject arg3, AviatorObject arg4, AviatorObject arg5, AviatorObject arg6,
      AviatorObject arg7, AviatorObject arg8, AviatorObject arg9, AviatorObject arg10,
      AviatorObject arg11, AviatorObject arg12, AviatorObject arg13) {
    return aviatorFunction.call(env, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10,
        arg11, arg12, arg13);
  }

  public AviatorObject call(Map<String, Object> env, AviatorObject arg1, AviatorObject arg2,
      AviatorObject arg3, AviatorObject arg4, AviatorObject arg5, AviatorObject arg6,
      AviatorObject arg7, AviatorObject arg8, AviatorObject arg9, AviatorObject arg10,
      AviatorObject arg11, AviatorObject arg12, AviatorObject arg13, AviatorObject arg14) {
    return aviatorFunction.call(env, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10,
        arg11, arg12, arg13, arg14);
  }

  public AviatorObject call(Map<String, Object> env, AviatorObject arg1, AviatorObject arg2,
      AviatorObject arg3, AviatorObject arg4, AviatorObject arg5, AviatorObject arg6,
      AviatorObject arg7, AviatorObject arg8, AviatorObject arg9, AviatorObject arg10,
      AviatorObject arg11, AviatorObject arg12, AviatorObject arg13, AviatorObject arg14,
      AviatorObject arg15) {
    return aviatorFunction.call(env, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10,
        arg11, arg12, arg13, arg14, arg15);
  }

  public AviatorObject call(Map<String, Object> env, AviatorObject arg1, AviatorObject arg2,
      AviatorObject arg3, AviatorObject arg4, AviatorObject arg5, AviatorObject arg6,
      AviatorObject arg7, AviatorObject arg8, AviatorObject arg9, AviatorObject arg10,
      AviatorObject arg11, AviatorObject arg12, AviatorObject arg13, AviatorObject arg14,
      AviatorObject arg15, AviatorObject arg16) {
    return aviatorFunction.call(env, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10,
        arg11, arg12, arg13, arg14, arg15, arg16);
  }

  public AviatorObject call(Map<String, Object> env, AviatorObject arg1, AviatorObject arg2,
      AviatorObject arg3, AviatorObject arg4, AviatorObject arg5, AviatorObject arg6,
      AviatorObject arg7, AviatorObject arg8, AviatorObject arg9, AviatorObject arg10,
      AviatorObject arg11, AviatorObject arg12, AviatorObject arg13, AviatorObject arg14,
      AviatorObject arg15, AviatorObject arg16, AviatorObject arg17) {
    return aviatorFunction.call(env, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10,
        arg11, arg12, arg13, arg14, arg15, arg16, arg17);
  }

  public AviatorObject call(Map<String, Object> env, AviatorObject arg1, AviatorObject arg2,
      AviatorObject arg3, AviatorObject arg4, AviatorObject arg5, AviatorObject arg6,
      AviatorObject arg7, AviatorObject arg8, AviatorObject arg9, AviatorObject arg10,
      AviatorObject arg11, AviatorObject arg12, AviatorObject arg13, AviatorObject arg14,
      AviatorObject arg15, AviatorObject arg16, AviatorObject arg17, AviatorObject arg18) {
    return aviatorFunction.call(env, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10,
        arg11, arg12, arg13, arg14, arg15, arg16, arg17, arg18);
  }

  public AviatorObject call(Map<String, Object> env, AviatorObject arg1, AviatorObject arg2,
      AviatorObject arg3, AviatorObject arg4, AviatorObject arg5, AviatorObject arg6,
      AviatorObject arg7, AviatorObject arg8, AviatorObject arg9, AviatorObject arg10,
      AviatorObject arg11, AviatorObject arg12, AviatorObject arg13, AviatorObject arg14,
      AviatorObject arg15, AviatorObject arg16, AviatorObject arg17, AviatorObject arg18,
      AviatorObject arg19) {
    return aviatorFunction.call(env, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10,
        arg11, arg12, arg13, arg14, arg15, arg16, arg17, arg18, arg19);
  }

  public AviatorObject call(Map<String, Object> env, AviatorObject arg1, AviatorObject arg2,
      AviatorObject arg3, AviatorObject arg4, AviatorObject arg5, AviatorObject arg6,
      AviatorObject arg7, AviatorObject arg8, AviatorObject arg9, AviatorObject arg10,
      AviatorObject arg11, AviatorObject arg12, AviatorObject arg13, AviatorObject arg14,
      AviatorObject arg15, AviatorObject arg16, AviatorObject arg17, AviatorObject arg18,
      AviatorObject arg19, AviatorObject arg20) {
    return aviatorFunction.call(env, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10,
        arg11, arg12, arg13, arg14, arg15, arg16, arg17, arg18, arg19, arg20);
  }

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
