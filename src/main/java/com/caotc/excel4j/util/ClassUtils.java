package com.caotc.excel4j.util;

import java.lang.reflect.Field;
import java.util.Arrays;
import java.util.Collection;
import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.Stream;

public class ClassUtils extends org.apache.commons.lang3.ClassUtils{
	private ClassUtils() {
		throw new AssertionError();
	}
	
	public static Stream<Field> getAllFieldStream(Class<?> type) {
		List<Class<?>> classes=getAllSuperclasses(type);
		classes.add(type);
		return classes.stream().map(Class::getDeclaredFields).map(Arrays::asList).flatMap(Collection::stream);
	}
	
	/**
	 * get all fields of the Class
	 * @param type Class Object
	 * @return all fields of the Class 
	 */
	public static Collection<Field> getAllFields(Class<?> type) {
		return getAllFieldStream(type).collect(Collectors.toList());
	}
}
