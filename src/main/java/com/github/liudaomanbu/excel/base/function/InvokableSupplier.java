package com.github.liudaomanbu.excel.base.function;

import java.lang.reflect.Constructor;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.util.function.Supplier;
import com.google.common.reflect.Invokable;

public class InvokableSupplier<T> implements Supplier<T> {
  public static <T> InvokableSupplier<T> of(Invokable<?, T> invokable) {
    return new InvokableSupplier<T>(invokable);
  }

  public static <T> InvokableSupplier<T> of(Constructor<T> constructor) {
    return of(Invokable.from(constructor));
  }
  
  @SuppressWarnings("unchecked")
  public static <T> InvokableSupplier<T> of(Method method) {
    return of((Invokable<?, T>)Invokable.from(method));
  }
  
  private Invokable<?, T> invokable;

  private InvokableSupplier(Invokable<?, T> invokable) {
    this.invokable = invokable;
  }

  @Override
  public T get() {
    try {
      return invokable.invoke(null);
    } catch (InvocationTargetException | IllegalAccessException e) {
      e.printStackTrace();
    }
    return null;
  }

}
