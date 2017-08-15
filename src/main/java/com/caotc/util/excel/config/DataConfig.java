package com.caotc.util.excel.config;

import com.caotc.util.excel.matcher.data.DataMatcher;

public class DataConfig<T extends Comparable<T>> {
	private Boolean nullAllow;
	private T maxValue;
	private T minValue;
	private Integer maxLength;
	private Integer minLength;
	private DataMatcher dataMatcher;
	
	public Boolean getNullAllow() {
		return nullAllow;
	}
	public void setNullAllow(Boolean nullAllow) {
		this.nullAllow = nullAllow;
	}
	public T getMaxValue() {
		return maxValue;
	}
	public void setMaxValue(T maxValue) {
		this.maxValue = maxValue;
	}
	public T getMinValue() {
		return minValue;
	}
	public void setMinValue(T minValue) {
		this.minValue = minValue;
	}
	public Integer getMaxLength() {
		return maxLength;
	}
	public void setMaxLength(Integer maxLength) {
		this.maxLength = maxLength;
	}
	public Integer getMinLength() {
		return minLength;
	}
	public void setMinLength(Integer minLength) {
		this.minLength = minLength;
	}
	public DataMatcher getDataMatcher() {
		return dataMatcher;
	}
	public void setDataMatcher(DataMatcher dataMatcher) {
		this.dataMatcher = dataMatcher;
	}
}
