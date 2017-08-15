package com.caotc.util.excel.config;

import java.util.Collection;

import com.caotc.util.excel.config.TableConfig.Direction;
import com.caotc.util.excel.matcher.data.DataMatcher;
import com.caotc.util.excel.matcher.data.value.StringValueMatcher;
import com.caotc.util.excel.matcher.data.value.StringValueMatcher.StringValueMatcherType;

public class MenuConfig{
	private static final StringValueMatcherType DEFAULT_STRING_MATCH_TYPE=StringValueMatcherType.EQUALS;
	private static final Boolean DEFAULT_MUST=Boolean.TRUE;
	//菜单匹配器
	private DataMatcher menuMatcher;
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
	
}