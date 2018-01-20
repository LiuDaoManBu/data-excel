package com.caotc.excel4j.config;

import java.lang.reflect.Field;
import java.util.List;
import java.util.Objects;
import java.util.Optional;
import java.util.function.Function;
import java.util.function.Predicate;
import java.util.stream.Stream;
import com.caotc.excel4j.constant.LoadType;
import com.caotc.excel4j.matcher.data.type.BaseDataType;
import com.caotc.excel4j.matcher.data.type.DataType;
import com.caotc.excel4j.parse.result.Menu;
import com.caotc.excel4j.parse.result.StandardCell;
import com.caotc.excel4j.validator.BaseValidator;
import com.caotc.excel4j.validator.Validator;
import com.google.common.base.Joiner;
import com.google.common.base.Preconditions;
import com.google.common.collect.ImmutableBiMap;
import com.google.common.collect.ImmutableList;
import com.google.common.collect.ImmutableMap;
import com.google.common.collect.Lists;
import com.google.common.reflect.TypeToken;

public class MenuDataConfig {
  public static class Builder {
    private MenuConfig menuConfig;
    private Field field;
    private BaseDataType baseDataType;
    private DataType dataType;
    private List<Validator<StandardCell>> validators;
    private String fieldName;
    private LoadType loadType;

    public Builder() {
      validators = Lists.newLinkedList();
    }

    public MenuDataConfig build() {
      dataType = Optional.ofNullable(dataType).orElse(baseDataType);
      return new MenuDataConfig(this);
    }

    public MenuConfig getMenuConfig() {
      return menuConfig;
    }

    public Builder setMenuConfig(MenuConfig menuConfig) {
      this.menuConfig = menuConfig;
      return this;
    }

    public Field getField() {
      return field;
    }

    public Builder setField(Field field) {
      this.field = field;
      return this;
    }

    public BaseDataType getBaseDataType() {
      return baseDataType;
    }

    public Builder setBaseDataType(BaseDataType baseDataType) {
      this.baseDataType = baseDataType;
      return this;
    }

    public DataType getDataType() {
      return dataType;
    }

    public Builder setDataType(DataType dataType) {
      this.dataType = dataType;
      return this;
    }

    public String getFieldName() {
      return fieldName;
    }

    public Builder setFieldName(String fieldName) {
      this.fieldName = fieldName;
      return this;
    }

    public LoadType getLoadType() {
      return loadType;
    }

    public Builder setLoadType(LoadType loadType) {
      this.loadType = loadType;
      return this;
    }

    public List<Validator<StandardCell>> getValidators() {
      return validators;
    }

    public Builder setValidators(List<Validator<StandardCell>> validators) {
      this.validators = validators;
      return this;
    }

  }

  private static final Joiner JOINER = Joiner.on("");
  private static final LoadType DEFAULT_LOAD_TYPE = LoadType.UNFIXED;
  // TODO 可配置
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

  public static Builder builder() {
    return new Builder();
  }

  private final MenuConfig menuConfig;
  private final LoadType loadType;
  private final Field field;
  private final String fieldName;
  private final DataType dataType;
  private final ImmutableList<Validator<StandardCell>> validators;

  protected MenuDataConfig(Builder builder) {
    menuConfig = builder.menuConfig;
    // TODO tip
    Preconditions.checkState(Objects.nonNull(menuConfig));
    loadType = Optional.ofNullable(builder.loadType).orElse(DEFAULT_LOAD_TYPE);
    field = builder.field;
    fieldName = Optional.ofNullable(builder.fieldName).orElse(field.getName());
    dataType = builder.dataType;
    // TODO tip
    Preconditions.checkState(Objects.nonNull(dataType));

    validators =
        Stream
            .concat(Stream.of(new BaseValidator<StandardCell>(
                ImmutableMap.<Predicate<StandardCell>, Function<StandardCell, String>>builder()
                    .put(cell -> dataType.test(cell.getValue()),
                        cell -> JOINER.join(cell.formatAsString(), "不符合",
                            DATA_TYPE_TO_TIPS.get(dataType), "格式"))// TODO tip
                    .build())),
                builder.validators.stream())
            .collect(ImmutableList.toImmutableList());

  }

  public <T> T cast(Object value, TypeToken<T> type) {
    return dataType.cast(value, type);
  }

  public ImmutableList<StandardCell> getDataCells(Menu menu) {
    return loadType.getDataCells(menu);
  }

  public LoadType getLoadType() {
    return loadType;
  }

  public Field getField() {
    return field;
  }

  public String getFieldName() {
    return fieldName;
  }

  public MenuConfig getMenuConfig() {
    return menuConfig;
  }

  public DataType getDataType() {
    return dataType;
  }

  public ImmutableList<Validator<StandardCell>> getValidators() {
    return validators;
  }
}
