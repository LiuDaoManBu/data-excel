package com.caotc.util.excel;
import org.apache.poi.ss.usermodel.Sheet;
import org.apache.poi.ss.usermodel.Workbook;

import com.alibaba.fastjson.JSONObject;

class A{
	private String id=null;
	private int value=-1;
	

	public String getId() {
		System.out.println("getId");
		return id;
	}
	public void setId(String id) {
		System.out.println("setId");
		this.id = id;
	}
	public int getValue() {
		System.out.println("getValue");
		return value;
	}
	public void setValue(int value) {
		System.out.println("setValue");
		this.value = value;
	}
	@Override
	public String toString() {
		return "A [id=" + id + ", value=" + value + "]";
	}
}
public class Test {
	public static void main(String[] args) {
		JSONObject jsonObject=new JSONObject();
		jsonObject.put("id", "aaaa");
		jsonObject.put("value", 123);
		System.out.println(jsonObject.toJavaObject(A.class));
		Workbook workbook=null;
		Sheet sheet=workbook.getSheetAt(0);
		
	}
}
