package com.github.liudaomanbu.excel.config;

import com.github.liudaomanbu.excel.convert.DateTimeFormatterAdapter;
import java.math.BigDecimal;
import java.time.format.DateTimeFormatter;
import java.time.format.DateTimeParseException;
import java.util.Collection;
import java.util.Date;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.Set;
import java.util.Vector;
import java.util.function.Function;
import java.util.function.Supplier;
import com.github.liudaomanbu.excel.base.function.InvokableSupplier;
import com.github.liudaomanbu.excel.constant.ConstructType;
import com.github.liudaomanbu.excel.util.ClassUtil;
import com.google.common.collect.ArrayListMultimap;
import com.google.common.collect.HashBasedTable;
import com.google.common.collect.Lists;
import com.google.common.collect.Maps;
import com.google.common.collect.Multimap;
import com.google.common.collect.Sets;
import com.google.common.collect.Table;
import com.google.common.reflect.TypeToken;

public final class ParserConfig {
  public static final ParserConfig GLOBAL = new ParserConfig();

  private final Map<Class<?>, Supplier<?>> classToSuppliers = Maps.newConcurrentMap();
  private final Map<Class<?>, ConstructType> classToConstructTypes = Maps.newConcurrentMap();
  private final Set<String> trueStrings=Sets.newConcurrentHashSet();
  private final Set<String> falseStrings=Sets.newConcurrentHashSet();
  private final Set<DateTimeFormatter> dateFormatters=Sets.newConcurrentHashSet();

  private final List<Function<String,Date>> stringToDateConverters=new Vector<>();

  public ParserConfig() {
    classToSuppliers.put(List.class, Lists::newArrayList);
    classToSuppliers.put(Set.class, Sets::newHashSet);
    classToSuppliers.put(Collection.class, Lists::newArrayList);
    classToSuppliers.put(Iterable.class, Lists::newArrayList);
    classToSuppliers.put(Multimap.class, ArrayListMultimap::create);
    classToSuppliers.put(Table.class, HashBasedTable::create);

    classToConstructTypes.put(List.class, ConstructType.ITERABLE);
    classToConstructTypes.put(Set.class, ConstructType.ITERABLE);
    classToConstructTypes.put(Collection.class, ConstructType.ITERABLE);
    classToConstructTypes.put(Iterable.class, ConstructType.ITERABLE);
    
    trueStrings.add("true");
    trueStrings.add("t");
    trueStrings.add("是");
    trueStrings.add("yes");
    trueStrings.add("y");
    
    falseStrings.add("false");
    trueStrings.add("f");
    falseStrings.add("否");
    falseStrings.add("no");
    falseStrings.add("n");

    dateFormatters.add(DateTimeFormatter.BASIC_ISO_DATE);
    dateFormatters.add(DateTimeFormatter.ISO_DATE);
    dateFormatters.add(DateTimeFormatter.ISO_DATE_TIME);
    dateFormatters.add(DateTimeFormatter.ISO_INSTANT);
    dateFormatters.add(DateTimeFormatter.ISO_LOCAL_DATE);
    dateFormatters.add(DateTimeFormatter.ISO_LOCAL_DATE_TIME);
    dateFormatters.add(DateTimeFormatter.ISO_LOCAL_TIME);
    dateFormatters.add(DateTimeFormatter.ISO_OFFSET_DATE);
    dateFormatters.add(DateTimeFormatter.ISO_OFFSET_DATE_TIME);
    dateFormatters.add(DateTimeFormatter.ISO_OFFSET_TIME);
    dateFormatters.add(DateTimeFormatter.ISO_ORDINAL_DATE);
    dateFormatters.add(DateTimeFormatter.ISO_TIME);
    dateFormatters.add(DateTimeFormatter.ISO_WEEK_DATE);
    dateFormatters.add(DateTimeFormatter.ISO_ZONED_DATE_TIME);
    dateFormatters.add(DateTimeFormatter.RFC_1123_DATE_TIME);

    registerConverter(DateTimeFormatterAdapter.create(DateTimeFormatter.RFC_1123_DATE_TIME));
    registerConverter(DateTimeFormatterAdapter.create(DateTimeFormatter.ISO_DATE_TIME));
    registerConverter(DateTimeFormatterAdapter.create(DateTimeFormatter.ISO_ZONED_DATE_TIME));
    registerConverter(DateTimeFormatterAdapter.create(DateTimeFormatter.ISO_INSTANT));
    registerConverter(DateTimeFormatterAdapter.create(DateTimeFormatter.ISO_OFFSET_DATE_TIME));
    registerConverter(DateTimeFormatterAdapter.create(DateTimeFormatter.ISO_LOCAL_DATE_TIME));
    registerConverter(DateTimeFormatterAdapter.create(DateTimeFormatter.ISO_OFFSET_DATE));
    registerConverter(DateTimeFormatterAdapter.create(DateTimeFormatter.ISO_LOCAL_DATE));
    registerConverter(DateTimeFormatterAdapter.create(DateTimeFormatter.ISO_DATE));
    registerConverter(DateTimeFormatterAdapter.create(DateTimeFormatter.ISO_ORDINAL_DATE));
    registerConverter(DateTimeFormatterAdapter.create(DateTimeFormatter.ISO_WEEK_DATE));
    registerConverter(DateTimeFormatterAdapter.create(DateTimeFormatter.BASIC_ISO_DATE));
    registerConverter(DateTimeFormatterAdapter.create(DateTimeFormatter.ISO_TIME));
    registerConverter(DateTimeFormatterAdapter.create(DateTimeFormatter.ISO_OFFSET_TIME));
    registerConverter(DateTimeFormatterAdapter.create(DateTimeFormatter.ISO_LOCAL_TIME));
  }

  public boolean isBoolean(boolean value){
    return true;
  }

  public boolean isBoolean(String value){
    return trueStrings.stream().anyMatch(s -> s.equalsIgnoreCase(value)) || falseStrings.stream().anyMatch(s -> s.equalsIgnoreCase(value));
  }

  public boolean isBoolean(Date value){
    return false;
  }

  public boolean isBoolean(double value){
    return false;
  }

  public boolean isString(boolean value){
    return true;
  }

  public boolean isString(String value){
    return true;
  }

  public boolean isString(double value){
    return true;
  }

  public boolean isString(Date value){
    return true;
  }

  public boolean isDouble(boolean value){
    return false;
  }

  public boolean isDouble(String value){
    try{
      new BigDecimal(value);
      return true;
    }catch (NumberFormatException e){
      return false;
    }
  }

  public boolean isDouble(double value){
    return true;
  }

  public boolean isDouble(Date value){
    return false;
  }

  public boolean isDate(boolean value){
    return false;
  }

  public boolean isDate(String value){
    return dateFormatters.stream().anyMatch(formatter->{
      try{
        formatter.parse(value);
        return true;
      }catch (DateTimeParseException e){
        return false;
      }
    });
  }

  public boolean isDate(double value){
    return false;
  }

  public boolean isDate(Date value){
    return true;
  }

  public boolean castToBoolean(String value){
    Optional<String> trueOptional=trueStrings.stream().filter(s -> s.equalsIgnoreCase(value)).findAny();
    if(trueOptional.isPresent()){
      return Boolean.TRUE;
    }

    Optional<String> falseOptional=falseStrings.stream().filter(s -> s.equalsIgnoreCase(value)).findAny();
    if(falseOptional.isPresent()){
      return Boolean.FALSE;
    }

    throw new IllegalArgumentException(value+" can't cast to boolean");
  }

  public boolean castToBoolean(double value){
    throw new IllegalArgumentException(value+" can't cast to boolean");
  }

  public boolean castToBoolean(Date value){
    throw new IllegalArgumentException(value+" can't cast to boolean");
  }

  public double castToDouble(boolean value){
    throw new IllegalArgumentException(value+" can't cast to double");
  }

  public double castToDouble(String value){
    return new BigDecimal(value).doubleValue();
  }

  public double castToDouble(Date value){
    throw new IllegalArgumentException(value+" can't cast to double");
  }

  public Date castToDate(boolean value){
    throw new IllegalArgumentException(value+" can't cast to Date");
  }

  public Date castToDate(String value){
    return stringToDateConverters.stream().filter(converter->{
      try {
        converter.apply(value);
      return true;
      }catch (DateTimeParseException e){
        return false;
      }
    }).findFirst().map(converter->converter.apply(value)).orElseThrow(()->new IllegalArgumentException(value+" can't cast to Date"));
  }

  public Date castToDate(double value){
    throw new IllegalArgumentException(value+"can't cast to Date");
  }

  public void registerConverter(Function<String,Date> converter){
    stringToDateConverters.add(converter);
  }

  public <T> void setConstructType(Class<T> type, ConstructType constructType) {
    classToConstructTypes.put(type, constructType);
  }

  public <T> ConstructType getConstructType(Class<T> type) {
    return getConstructType(TypeToken.of(type));
  }

  public <T> ConstructType getConstructType(TypeToken<T> token) {
    if (token.isArray()) {
      return ConstructType.ITERABLE;
    }
    // 父子关系生效?
    Optional<ConstructType> optional = token.getTypes().rawTypes().stream().filter(
        type -> classToConstructTypes.containsKey(type))
        .findFirst().map(classToConstructTypes::get);
    return optional.orElse(ConstructType.OBJECT);
  }

  public <T> void setSupplier(Class<T> type, Supplier<T> supplier) {
    classToSuppliers.put(type, supplier);
  }

  public <T> Optional<Supplier<T>> getSupplier(Class<T> type) {
    return getSupplier(TypeToken.of(type));
  }

  @SuppressWarnings("unchecked")
  public <T> Optional<Supplier<T>> getSupplier(TypeToken<T> token) {
    // 父子关系生效?
    Optional<Supplier<T>> optional =
        token.getTypes().rawTypes().stream().filter(classToSuppliers::containsKey).findFirst()
            .map(classToSuppliers::get).map(supplier -> (Supplier<T>) supplier);
    return optional.isPresent() ? optional
        : ClassUtil.getDefaultConstructor((Class<T>) token.getRawType()).map(InvokableSupplier::of);
  }

  public <T> Optional<T> newInstance(Class<T> type) {
    return newInstance(TypeToken.of(type));
  }

  public <T> Optional<T> newInstance(TypeToken<T> token) {
    return getSupplier(token).map(Supplier::get);
  }
}
