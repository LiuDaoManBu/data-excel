package com.caotc.excel4j;
import java.lang.reflect.Field;
import java.util.Collection;
import java.util.Map;
import java.util.Set;

import com.caotc.excel4j.matcher.data.type.NaturalDataType;
import com.google.common.collect.Lists;
import com.google.common.collect.Maps;

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
class B{
	private final int id;
	private final Collection<Integer> values;
	public B() {
		super();
		this.id = 1111;
		values=null;
	}
	@Override
	public String toString() {
		return "B [id=" + id + ", values=" + values + "]";
	}
	public int getId() {
		return id;
	}
	public Collection<Integer> getValues() {
		return values;
	}

}
public class Test {
	public static void main(String[] args) throws Exception{
//		JSONObject jsonObject=new JSONObject();
//		jsonObject.put("id", "1");
//		System.out.println(JSONObject.toJavaObject(jsonObject, A.class));
		B b=new B();
		System.out.println(b);
		Field field=B.class.getDeclaredField("values");
		field.setAccessible(true);
		field.set(b, Lists.newArrayList("a","b"));
		System.out.println(b);
		field.set(b, Lists.newArrayList(111,123));
		System.out.println(b);
//		Integer first=b.getValues().iterator().next();
//		System.out.println(first);
//		System.out.println(ClassUtils.getAllFields(B.class).stream().collect(Collectors.toMap(Field::getName, Function.identity())));
//		JSONObject jsonObject=new JSONObject();
//		jsonObject.put("value1", 1);
//		jsonObject.put("value2", 2);
//		jsonObject.put("value3", 3);
//		jsonObject.forEach((key,value)->{jsonObject.put(key, (int)value*10);});
//		System.out.println(jsonObject);
//		Class<?> type=null;
//		System.out.println(getValue(type));
	}
	
	public static <T> T getValue(Class<T> type) {
		Object value=null;
		if(value!=null && !value.getClass().equals(type)) {
			value=NaturalDataType.DECIMAL.cast(value, type);
		}
		return (T) value;
	}
}
