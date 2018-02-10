package com.github.liudaomanbu.excel.script.runtime.type;

import java.util.Map;
import com.github.liudaomanbu.excel.script.constant.ScriptType;
import com.googlecode.aviator.exception.ExpressionRuntimeException;
import com.googlecode.aviator.utils.TypeUtils;

public abstract class BaseScriptObject {
  public abstract int compare(BaseScriptObject other, Map<String, Object> env);

  public abstract ScriptType getScriptType();

  public boolean isNull(Map<String, Object> env) {
    return this.getValue(env) == null;
  }

  public BaseScriptObject match(BaseScriptObject other, Map<String, Object> env) {
    throw new ExpressionRuntimeException(this.desc(env) + " doesn't support match operation '=~'");
  }


  public BaseScriptObject neg(Map<String, Object> env) {
    throw new ExpressionRuntimeException(
        this.desc(env) + " doesn't support negative operation '-'");
  }


  public BaseScriptObject not(Map<String, Object> env) {
    throw new ExpressionRuntimeException(this.desc(env) + " doesn't support not operation '!'");
  }


  public String desc(Map<String, Object> env) {
    return this.getScriptType() + "(" + this.getValue(env) + ")";
  }


  public abstract Object getValue(Map<String, Object> env);


  public BaseScriptObject add(BaseScriptObject other, Map<String, Object> env) {
    throw new ExpressionRuntimeException(
        "Could not add " + this.desc(env) + " with " + other.desc(env));
  }


  public BaseScriptObject bitAnd(BaseScriptObject other, Map<String, Object> env) {
    throw new ExpressionRuntimeException(
        "Could not bitAnd " + this.desc(env) + " with " + other.desc(env));
  }


  public BaseScriptObject bitOr(BaseScriptObject other, Map<String, Object> env) {
    throw new ExpressionRuntimeException(
        "Could not bitOr " + this.desc(env) + " with " + other.desc(env));
  }


  public BaseScriptObject bitXor(BaseScriptObject other, Map<String, Object> env) {
    throw new ExpressionRuntimeException(
        "Could not bitXor " + this.desc(env) + " with " + other.desc(env));
  }


  public BaseScriptObject shiftRight(BaseScriptObject other, Map<String, Object> env) {
    throw new ExpressionRuntimeException(
        "Could not shiftRight " + this.desc(env) + " with " + other.desc(env));
  }


  public BaseScriptObject shiftLeft(BaseScriptObject other, Map<String, Object> env) {
    throw new ExpressionRuntimeException(
        "Could not shiftLeft " + this.desc(env) + " with " + other.desc(env));
  }


  public BaseScriptObject unsignedShiftRight(BaseScriptObject other, Map<String, Object> env) {
    throw new ExpressionRuntimeException(
        "Could not unsignedShiftRight " + this.desc(env) + " with " + other.desc(env));
  }


  public BaseScriptObject bitNot(Map<String, Object> env) {
    throw new ExpressionRuntimeException(this.desc(env) + " doesn't support not operation '^'");
  }


  public BaseScriptObject sub(BaseScriptObject other, Map<String, Object> env) {
    throw new ExpressionRuntimeException(
        "Could not sub " + this.desc(env) + " with " + other.desc(env));
  }


  public BaseScriptObject mod(BaseScriptObject other, Map<String, Object> env) {
    throw new ExpressionRuntimeException(
        "Could not mod " + this.desc(env) + " with " + other.desc(env));
  }


  public BaseScriptObject div(BaseScriptObject other, Map<String, Object> env) {
    throw new ExpressionRuntimeException(
        "Could not div " + this.desc(env) + " with " + other.desc(env));
  }


  public BaseScriptObject mult(BaseScriptObject other, Map<String, Object> env) {
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

  public BaseScriptObject getElement(Map<String, Object> env, BaseScriptObject indexObject) {
    throw new ExpressionRuntimeException(this.desc(env) + " is not a array");
  }
}
