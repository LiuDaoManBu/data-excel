package com.github.liudaomanbu.excel.parse.result;

import com.github.liudaomanbu.excel.config.TableDataConfig;
import com.github.liudaomanbu.excel.matcher.data.type.BaseDataType;
import com.github.liudaomanbu.excel.parse.error.ValidationError;
import com.github.liudaomanbu.excel.validator.BaseValidator;
import com.github.liudaomanbu.excel.validator.Validator;
import com.google.common.base.Joiner;
import com.google.common.collect.ImmutableBiMap;
import com.google.common.collect.ImmutableList;
import java.util.Collection;
import java.util.Map;
import java.util.Optional;
import java.util.function.Consumer;
import com.alibaba.fastjson.JSONObject;
import com.github.liudaomanbu.excel.util.ExcelUtil;
import java.util.stream.Stream;

public class Data<T> {

  private static final Joiner JOINER = Joiner.on("").skipNulls();
  private static final ImmutableBiMap<BaseDataType, String> DATA_TYPE_TO_TIPS =
      ImmutableBiMap.<BaseDataType, String>builder().put(BaseDataType.BOOLEAN, "是否")
          .put(BaseDataType.CHINESE, "中文").put(BaseDataType.DATE, "日期")
          .put(BaseDataType.DATE_TIME, "日期时间").put(BaseDataType.DECIMAL, "小数")
          .put(BaseDataType.EMAIL, "邮箱").put(BaseDataType.ENGLISH, "英语")
          .put(BaseDataType.ENGLISH_OR_NUMBER, "英语或数字").put(BaseDataType.ENUM, "枚举")
          .put(BaseDataType.ID_CARD_NUMBER, "身份证号码").put(BaseDataType.NATURAL_NUMBER, "自然数")
          .put(BaseDataType.NEGATIVE_DECIMAL, "负小数").put(BaseDataType.NEGATIVE_NUMBER, "负数")
          .put(BaseDataType.NEGATIVE_WHOLE_NUMBER, "负整数").put(BaseDataType.NUMBER, "数字")
          .put(BaseDataType.PHONE, "电话号码").put(BaseDataType.POSITIVE_DECIMAL, "正小数")
          .put(BaseDataType.POSITIVE_NUMBER, "正数").put(BaseDataType.POSITIVE_WHOLE_NUMBER, "正整数")
          .put(BaseDataType.STRING, "字符串").put(BaseDataType.TELEPHONE, "手机号码")
          .put(BaseDataType.TIME, "时间").put(BaseDataType.WHOLE_NUMBER, "整数").build();

  private final TableData<T> tableData;
  private final Map<Menu<T>, StandardCell> menuToValueCells;
  private final Map<String, Object> json;
  private final T value;
  private final ImmutableList<ValidationError<Data<T>>> errors;

  public Data(TableData<T> tableData, Map<Menu<T>, StandardCell> menuToValueCells) {
    this.tableData = tableData;
    this.menuToValueCells = menuToValueCells;
    json = ExcelUtil.toJsonObject(menuToValueCells);
    tableData.getConfig().getBeforeTransform().accept(json);
    value = new JSONObject(json).toJavaObject(tableData.getConfig().getType());
    tableData.getConfig().getBeforeValidator().accept(value);
    errors = getValidators()
        .filter(validator -> validator.premise(this)).map(validator -> validator.validate(this))
        .flatMap(
            Collection::stream).collect(ImmutableList.toImmutableList());
  }

  private Stream<Validator<Data<T>>> getValidators() {
    return Stream
        .concat(createDataValidator(), Optional.ofNullable(tableData).map(TableData::getConfig)
            .map(TableDataConfig::getValidators).map(Collection::stream).orElseGet(Stream::empty));
  }

  private Stream<Validator<Data<T>>> createDataValidator() {
    return tableData.getTable().getDataMenus()
        .map(menu -> new BaseValidator<Data<T>>(
            data -> menu.getData().getConfig().getDataType()
                .test(data.getMenuToValueCells().get(menu).getValue()),
            data -> JOINER.join(menu.getFullName(),
                data.getMenuToValueCells().get(menu).formatAsString(), "单元格",
                data.getMenuToValueCells().get(menu).getValue(), "不符合",
                DATA_TYPE_TO_TIPS.get(menu.getData().getConfig().getDataType()), "格式")));
  }

  public Menu<T> getMenuByFieldName(String fieldName) {
    return menuToValueCells.keySet().stream().filter(menu -> menu.getFieldName().equals(fieldName))
        .findAny().orElse(null);
  }

  public TableData<T> getTableData() {
    return tableData;
  }

  public Map<Menu<T>, StandardCell> getMenuToValueCells() {
    return menuToValueCells;
  }

  public Map<String, Object> getJson() {
    return json;
  }

  public T getValue() {
    return value;
  }

  public ImmutableList<ValidationError<Data<T>>> getErrors() {
    return errors;
  }
}
