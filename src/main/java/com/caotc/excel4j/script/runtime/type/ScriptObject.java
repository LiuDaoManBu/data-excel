package com.caotc.excel4j.script.runtime.type;

import java.util.Map;
import com.caotc.excel4j.script.constant.ScriptType;
import com.googlecode.aviator.exception.ExpressionRuntimeException;
import com.googlecode.aviator.utils.TypeUtils;

public abstract class ScriptObject {
  public abstract int compare(ScriptObject other, Map<String, Object> env);

  public abstract ScriptType getScriptType();

  public boolean isNull(Map<String, Object> env) {
    return this.getValue(env) == null;
  }

  public ScriptObject match(ScriptObject other, Map<String, Object> env) {
    throw new ExpressionRuntimeException(this.desc(env) + " doesn't support match operation '=~'");
  }


  public ScriptObject neg(Map<String, Object> env) {
    throw new ExpressionRuntimeException(
        this.desc(env) + " doesn't support negative operation '-'");
  }


  public ScriptObject not(Map<String, Object> env) {
    throw new ExpressionRuntimeException(this.desc(env) + " doesn't support not operation '!'");
  }


  public String desc(Map<String, Object> env) {
    return this.getScriptType() + "(" + this.getValue(env) + ")";
  }


  public abstract Object getValue(Map<String, Object> env);


  public ScriptObject add(ScriptObject other, Map<String, Object> env) {
    throw new ExpressionRuntimeException(
        "Could not add " + this.desc(env) + " with " + other.desc(env));
  }


  public ScriptObject bitAnd(ScriptObject other, Map<String, Object> env) {
    throw new ExpressionRuntimeException(
        "Could not bitAnd " + this.desc(env) + " with " + other.desc(env));
  }


  public ScriptObject bitOr(ScriptObject other, Map<String, Object> env) {
    throw new ExpressionRuntimeException(
        "Could not bitOr " + this.desc(env) + " with " + other.desc(env));
  }


  public ScriptObject bitXor(ScriptObject other, Map<String, Object> env) {
    throw new ExpressionRuntimeException(
        "Could not bitXor " + this.desc(env) + " with " + other.desc(env));
  }


  public ScriptObject shiftRight(ScriptObject other, Map<String, Object> env) {
    throw new ExpressionRuntimeException(
        "Could not shiftRight " + this.desc(env) + " with " + other.desc(env));
  }


  public ScriptObject shiftLeft(ScriptObject other, Map<String, Object> env) {
    throw new ExpressionRuntimeException(
        "Could not shiftLeft " + this.desc(env) + " with " + other.desc(env));
  }


  public ScriptObject unsignedShiftRight(ScriptObject other, Map<String, Object> env) {
    throw new ExpressionRuntimeException(
        "Could not unsignedShiftRight " + this.desc(env) + " with " + other.desc(env));
  }


  public ScriptObject bitNot(Map<String, Object> env) {
    throw new ExpressionRuntimeException(this.desc(env) + " doesn't support not operation '^'");
  }


  public ScriptObject sub(ScriptObject other, Map<String, Object> env) {
    throw new ExpressionRuntimeException(
        "Could not sub " + this.desc(env) + " with " + other.desc(env));
  }


  public ScriptObject mod(ScriptObject other, Map<String, Object> env) {
    throw new ExpressionRuntimeException(
        "Could not mod " + this.desc(env) + " with " + other.desc(env));
  }


  public ScriptObject div(ScriptObject other, Map<String, Object> env) {
    throw new ExpressionRuntimeException(
        "Could not div " + this.desc(env) + " with " + other.desc(env));
  }


  public ScriptObject mult(ScriptObject other, Map<String, Object> env) {
    throw new ExpressionRuntimeException(
        "Could not mult " + this.desc(env) + " with " + other.desc(env));
  }


  public Number numberValue(Map<String, Object> env) {
    if (!(this.getValue(env) instanceof Number)) {
      throw new ExpressionRuntimeException(this.desc(env) + " is not a number value");
    }
    return (Number) this.getValue(env);
  }


  public String stringValue(Map<String, Object> env) {
    Object value = this.getValue(env);
    if (!(TypeUtils.isString(value))) {
      throw new ExpressionRuntimeException(this.desc(env) + " is not a string value");
    }
    return String.valueOf(value);
  }


  public boolean booleanValue(Map<String, Object> env) {
    if (!(this.getValue(env) instanceof Boolean)) {
      throw new ExpressionRuntimeException(this.desc(env) + " is not a boolean value");
    }
    return (Boolean) this.getValue(env);
  }

  public ScriptObject getElement(Map<String, Object> env, ScriptObject indexObject) {
    throw new ExpressionRuntimeException(this.desc(env) + " is not a array");
  }
}
