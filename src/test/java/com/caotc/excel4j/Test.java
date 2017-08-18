package com.caotc.excel4j;
import java.util.Set;

import com.alibaba.fastjson.JSONObject;
import com.caotc.excel4j.matcher.data.type.DataType;
import com.caotc.excel4j.matcher.data.type.NaturalDataType;

class A{
	private String id=null;
	private Set<Integer> values;
	

	public String getId() {
		System.out.println("getId");
		return id;
	}
	public void setId(String id) {
		System.out.println("setId");
		this.id = id;
	}
	public Set<Integer> getValues() {
		return values;
	}
	public void setValues(Set<Integer> values) {
		this.values = values;
	}
	@Override
	public String toString() {
		return "A [id=" + id + ", values=" + values + "]";
	}
}
class B extends A{
	private int id;

//	public int getId() {
//		return id;
//	}

	public void setId(int id) {
		this.id = id;
	}
	
}
public class Test {
	public static void main(String[] args) {
//		JSONObject jsonObject=new JSONObject();
//		jsonObject.put("id", "1");
//		System.out.println(JSONObject.toJavaObject(jsonObject, B.class));
//		System.out.println(ClassUtils.getAllFields(B.class).stream().collect(Collectors.toMap(Field::getName, Function.identity())));
//		JSONObject jsonObject=new JSONObject();
//		jsonObject.put("value1", 1);
//		jsonObject.put("value2", 2);
//		jsonObject.put("value3", 3);
//		jsonObject.forEach((key,value)->{jsonObject.put(key, (int)value*10);});
//		System.out.println(jsonObject);
		Class<?> type=null;
		System.out.println(getValue(type));
	}
	
	public static <T> T getValue(Class<T> type) {
		Object value=null;
		if(value!=null && !value.getClass().equals(type)) {
			value=NaturalDataType.DECIMAL.cast(value, type);
		}
		return (T) value;
	}
}
