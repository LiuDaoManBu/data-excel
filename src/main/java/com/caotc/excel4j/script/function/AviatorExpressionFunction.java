package com.caotc.excel4j.script.function;

import java.util.Map;
import com.googlecode.aviator.runtime.type.AviatorFunction;
import com.googlecode.aviator.runtime.type.AviatorObject;

public class AviatorExpressionFunction implements ExpressionFunction,AviatorFunction {
  public static AviatorExpressionFunction valueOf(AviatorFunction aviatorFunction) {
    return new AviatorExpressionFunction(aviatorFunction);
  }

  private AviatorFunction aviatorFunction;

  private AviatorExpressionFunction(AviatorFunction aviatorFunction) {
    this.aviatorFunction = aviatorFunction;
  }

  public String getName() {
    return aviatorFunction.getName();
  }

  @Override
  public AviatorObject call(Map<String, Object> env, AviatorObject... args) {
    switch (args.length) {
      case 0:
        return aviatorFunction.call(env);
      case 1:
        return aviatorFunction.call(env, args[0]);
      case 2:
        return aviatorFunction.call(env, args[0], args[1]);
      case 3:
        return aviatorFunction.call(env, args[0], args[1], args[2]);
      case 4:
        return aviatorFunction.call(env, args[0], args[1], args[2], args[3]);
      case 5:
        return aviatorFunction.call(env, args[0], args[1], args[2], args[3], args[4]);
      case 6:
        return aviatorFunction.call(env, args[0], args[1], args[2], args[3], args[4], args[5]);
      case 7:
        return aviatorFunction.call(env, args[0], args[1], args[2], args[3], args[4], args[5],
            args[6]);
      case 8:
        return aviatorFunction.call(env, args[0], args[1], args[2], args[3], args[4], args[5],
            args[6], args[7]);
      case 9:
        return aviatorFunction.call(env, args[0], args[1], args[2], args[3], args[4], args[5],
            args[6], args[7], args[8]);
      case 10:
        return aviatorFunction.call(env, args[0], args[1], args[2], args[3], args[4], args[5],
            args[6], args[7], args[8], args[9]);
      case 11:
        return aviatorFunction.call(env, args[0], args[1], args[2], args[3], args[4], args[5],
            args[6], args[7], args[8], args[9], args[10]);
      case 12:
        return aviatorFunction.call(env, args[0], args[1], args[2], args[3], args[4], args[5],
            args[6], args[7], args[8], args[9], args[10], args[11]);
      case 13:
        return aviatorFunction.call(env, args[0], args[1], args[2], args[3], args[4], args[5],
            args[6], args[7], args[8], args[9], args[10], args[11], args[12]);
      case 14:
        return aviatorFunction.call(env, args[0], args[1], args[2], args[3], args[4], args[5],
            args[6], args[7], args[8], args[9], args[10], args[11], args[12], args[13]);
      case 15:
        return aviatorFunction.call(env, args[0], args[1], args[2], args[3], args[4], args[5],
            args[6], args[7], args[8], args[9], args[10], args[11], args[12], args[13], args[14]);
      case 16:
        return aviatorFunction.call(env, args[0], args[1], args[2], args[3], args[4], args[5],
            args[6], args[7], args[8], args[9], args[10], args[11], args[12], args[13], args[14],
            args[15]);
      case 17:
        return aviatorFunction.call(env, args[0], args[1], args[2], args[3], args[4], args[5],
            args[6], args[7], args[8], args[9], args[10], args[11], args[12], args[13], args[14],
            args[15], args[16]);
      case 18:
        return aviatorFunction.call(env, args[0], args[1], args[2], args[3], args[4], args[5],
            args[6], args[7], args[8], args[9], args[10], args[11], args[12], args[13], args[14],
            args[15], args[16], args[17]);
      case 19:
        return aviatorFunction.call(env, args[0], args[1], args[2], args[3], args[4], args[5],
            args[6], args[7], args[8], args[9], args[10], args[11], args[12], args[13], args[14],
            args[15], args[16], args[17], args[18]);
      case 20:
        return aviatorFunction.call(env, args[0], args[1], args[2], args[3], args[4], args[5],
            args[6], args[7], args[8], args[9], args[10], args[11], args[12], args[13], args[14],
            args[15], args[16], args[17], args[18], args[19]);
      default:
        AviatorObject[] variableArgs = new AviatorObject[args.length - 20];
        System.arraycopy(args, 0, variableArgs, 0, variableArgs.length);
        return aviatorFunction.call(env, args[0], args[1], args[2], args[3], args[4], args[5],
            args[6], args[7], args[8], args[9], args[10], args[11], args[12], args[13], args[14],
            args[15], args[16], args[17], args[18], args[19], variableArgs);
    }
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
