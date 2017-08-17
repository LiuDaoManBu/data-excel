package com.caotc.excel4j.config;

import java.util.Collection;

import com.caotc.excel4j.constant.Direction;
import com.caotc.excel4j.matcher.data.DataMatcher;
import com.caotc.excel4j.matcher.data.value.StringValueMatcher.StringValueMatcherType;
import com.caotc.excel4j.matcher.usermodel.CellMatcher;

public class MenuConfig{
	private static final StringValueMatcherType DEFAULT_STRING_MATCH_TYPE=StringValueMatcherType.EQUALS;
	private static final Boolean DEFAULT_MUST=Boolean.TRUE;
	//菜单匹配器
	private CellMatcher menuMatcher;
	private DataMatcher dataMatcher;
	//数据单元格相对于菜单的方向
	private Direction direction;	
	//第一个数据单元格相对于菜单单元格的单元格距离
	private Integer firstDataDistance;
	private Boolean data;
	private Boolean singleData;
	private Boolean must;
	private Boolean dynamic;
	//属性名字
	private String fieldName;
	private MenuConfig parentMenuConfig;
	private Collection<MenuConfig> childrenMenuConfigs;
	public CellMatcher getMenuMatcher() {
		return menuMatcher;
	}
	public void setMenuMatcher(CellMatcher menuMatcher) {
		this.menuMatcher = menuMatcher;
	}
	public DataMatcher getDataMatcher() {
		return dataMatcher;
	}
	public void setDataMatcher(DataMatcher dataMatcher) {
		this.dataMatcher = dataMatcher;
	}
	public Direction getDirection() {
		return direction;
	}
	public void setDirection(Direction direction) {
		this.direction = direction;
	}
	public Integer getFirstDataDistance() {
		return firstDataDistance;
	}
	public void setFirstDataDistance(Integer firstDataDistance) {
		this.firstDataDistance = firstDataDistance;
	}
	public Boolean getData() {
		return data;
	}
	public void setData(Boolean data) {
		this.data = data;
	}
	public Boolean getSingleData() {
		return singleData;
	}
	public void setSingleData(Boolean singleData) {
		this.singleData = singleData;
	}
	public Boolean getMust() {
		return must;
	}
	public void setMust(Boolean must) {
		this.must = must;
	}
	public Boolean getDynamic() {
		return dynamic;
	}
	public void setDynamic(Boolean dynamic) {
		this.dynamic = dynamic;
	}
	public String getFieldName() {
		return fieldName;
	}
	public void setFieldName(String fieldName) {
		this.fieldName = fieldName;
	}
	public MenuConfig getParentMenuConfig() {
		return parentMenuConfig;
	}
	public void setParentMenuConfig(MenuConfig parentMenuConfig) {
		this.parentMenuConfig = parentMenuConfig;
	}
	public Collection<MenuConfig> getChildrenMenuConfigs() {
		return childrenMenuConfigs;
	}
	public void setChildrenMenuConfigs(Collection<MenuConfig> childrenMenuConfigs) {
		this.childrenMenuConfigs = childrenMenuConfigs;
	}
	
	
}