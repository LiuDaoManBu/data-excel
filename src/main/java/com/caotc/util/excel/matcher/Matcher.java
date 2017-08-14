package com.caotc.util.excel.matcher;

public interface Matcher {
	boolean support(Object value);
	boolean matches(Object value);
}
