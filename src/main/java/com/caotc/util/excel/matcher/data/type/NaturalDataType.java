package com.caotc.util.excel.matcher.data.type;

import java.math.BigDecimal;
import java.math.BigInteger;
import java.util.Calendar;
import java.util.Collection;
import java.util.Collections;
import java.util.Date;
import java.util.function.Supplier;

import com.alibaba.fastjson.JSONException;
import com.alibaba.fastjson.util.TypeUtils;
import com.google.common.collect.Sets;

public enum NaturalDataType implements DataType{
	DECIMAL(float.class,Float.class,double.class,Double.class,BigDecimal.class){
		@Override
		public boolean matches(Object value) {
			try{
				return TypeUtils.castToBigDecimal(value).scale()>ZERO;
			}catch(NumberFormatException e){
				return Boolean.FALSE;
			}
		}
	},WHOLE_NUMBER(() -> {
		Collection<Class<?>> classes=DECIMAL.canCastClasses();
		Collections.addAll(classes, byte.class,Byte.class,short.class,Short.class,int.class,Integer.class,long
				.class,Long.class,BigInteger.class);
		return classes;
	}){
		@Override
		public boolean matches(Object value) {
			try{
				TypeUtils.castToBigDecimal(value).toBigIntegerExact();
				return Boolean.TRUE;
			}catch(ArithmeticException | NumberFormatException e){
				return Boolean.FALSE;
			}
		}
	},NUMBER(DECIMAL.canCastClasses()) {
		@Override
		public boolean matches(Object value) {
			return DECIMAL.matches(value) || WHOLE_NUMBER.matches(value);
		}
	},POSITIVE_NUMBER(float.class,Float.class,double.class,Double.class,BigDecimal.class) {
		@Override
		public boolean matches(Object value) {
			try{
				return NUMBER.matches(value) && TypeUtils.castToBigDecimal(value).compareTo(BigDecimal.ZERO)>ZERO;
			}catch(NumberFormatException e){
				return Boolean.FALSE;
			}
		}
	},NEGATIVE_NUMBER(float.class,Float.class,double.class,Double.class,BigDecimal.class) {
		@Override
		public boolean matches(Object value) {
			try{
				return NUMBER.matches(value) && TypeUtils.castToBigDecimal(value).compareTo(BigDecimal.ZERO)<ZERO;
			}catch(NumberFormatException e){
				return Boolean.FALSE;
			}
		}
	},POSITIVE_WHOLE_NUMBER(() -> {
		Collection<Class<?>> classes=POSITIVE_NUMBER.canCastClasses();
		classes.retainAll(WHOLE_NUMBER.canCastClasses());
		return classes;
	}){
		@Override
		public boolean matches(Object value) {
			return POSITIVE_NUMBER.matches(value) && WHOLE_NUMBER.matches(value);
		}
	},NEGATIVE_WHOLE_NUMBER(() -> {
		Collection<Class<?>> classes=NEGATIVE_NUMBER.canCastClasses();
		classes.retainAll(WHOLE_NUMBER.canCastClasses());
		return classes;
	}){
		@Override
		public boolean matches(Object value) {
			return NEGATIVE_NUMBER.matches(value) && WHOLE_NUMBER.matches(value);
		}
	},NATURAL_NUMBER(POSITIVE_WHOLE_NUMBER.canCastClasses()) {
		@Override
		public boolean matches(Object value) {
			try{
				return POSITIVE_WHOLE_NUMBER.matches(value) || WHOLE_NUMBER.cast(value,BigInteger.class)
						.equals(BigInteger.ZERO);
			}catch(ArithmeticException | NumberFormatException e){
				return Boolean.FALSE;
			}
		}
	},POSITIVE_DECIMAL(() -> {
		Collection<Class<?>> classes=POSITIVE_NUMBER.canCastClasses();
		classes.retainAll(DECIMAL.canCastClasses());
		return classes;
	}){
		@Override
		public boolean matches(Object value) {
			return DECIMAL.matches(value) && POSITIVE_NUMBER.matches(value);
		}
	},NEGATIVE_DECIMAL(() -> {
		Collection<Class<?>> classes=NEGATIVE_NUMBER.canCastClasses();
		classes.retainAll(DECIMAL.canCastClasses());
		return classes;
	}){
		@Override
		public boolean matches(Object value) {
			return DECIMAL.matches(value) && NEGATIVE_NUMBER.matches(value);
		}
	},STRING(String.class) {
		@Override
		public boolean matches(Object value) {
			return Boolean.TRUE;
		}
		
		@SuppressWarnings("unchecked")
		@Override
		public <T> T cast(Object value,Class<T> clazz){
			if(String.class.equals(clazz)){
				return (T) TypeUtils.castToString(value);
			}
			return super.cast(value,clazz);
		}
	},DATE(Date.class,Calendar.class){
		@Override
		public boolean matches(Object value) {
			try{
				TypeUtils.castToDate(value);
				return Boolean.TRUE;
			}catch(JSONException e){
				return Boolean.FALSE;
			}
		}
	};
	private static final int ZERO=0;
	private final Collection<Class<?>> classes=Sets.newHashSet();
	private NaturalDataType(Class<?>... classes){
		Collections.addAll(this.classes, classes);
	}
	
	private NaturalDataType(Collection<Class<?>> classes){
		this.classes.addAll(classes);
	}
	
	private NaturalDataType(Supplier<Collection<Class<?>>> supplier){
		classes.addAll(supplier.get());
	}
	
	@Override
	public boolean support(Object value){
		return Boolean.TRUE;
	}
	
	@Override
	public abstract boolean matches(Object value);
	
	@Override
	@SuppressWarnings("unchecked")
	public Collection<Class<?>> canCastClasses(){
		Collection<Class<?>> copy = null;
		try {
			copy=classes.getClass().newInstance();
			copy.addAll(classes);
		} catch (InstantiationException | IllegalAccessException e) {
			e.printStackTrace();
		}	
		return copy;
	}
	
	@Override
	public <T> boolean canCast(Class<T> clazz){
		return classes.contains(clazz);
	}
	
	@SuppressWarnings("unchecked")
	@Override
	public <T> T cast(Object value,Class<T> clazz){
		if(Sets.newHashSet(byte.class,Byte.class,short.class,Short.class,int.class,Integer.class,long
				.class,Long.class,BigInteger.class).contains(clazz)){
			value=TypeUtils.castToBigDecimal(value).toBigIntegerExact();
		}
		return TypeUtils.castToJavaBean(value, clazz);
	}
}
