package com.caotc.util.excel.matcher;

import java.util.Collection;



public interface DataType extends Matcher{
	abstract Collection<Class<?>> canCastClasses();
	abstract <T> boolean canCast(Class<T> clazz);
	abstract <T> T cast(Object value,Class<T> clazz);
}
