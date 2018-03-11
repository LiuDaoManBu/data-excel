package com.github.liudaomanbu.excel.convert;

/**
 * @author liudaomanbu
 * @date 2018-03-11 20:32
 **/
public interface Converter<T,R> {
  R convert(T value);
}
