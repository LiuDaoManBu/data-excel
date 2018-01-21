package com.github.liudaomanbu.excel.config;

import java.util.Collection;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.Set;
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

public class ParserConfig {
  public static final ParserConfig GLOBAL = new ParserConfig();

  private final Map<Class<?>, Supplier<?>> classToSuppliers = Maps.newHashMap();
  private final Map<Class<?>, ConstructType> classToConstructTypes = Maps.newHashMap();

  public ParserConfig() {
    setSupplier(List.class, Lists::newArrayList);
    setSupplier(Set.class, Sets::newHashSet);
    setSupplier(Collection.class, Lists::newArrayList);
    setSupplier(Iterable.class, Lists::newArrayList);
    setSupplier(Multimap.class, ArrayListMultimap::create);
    setSupplier(Table.class, HashBasedTable::create);
    
    setConstructType(List.class,ConstructType.ITERABLE);
    setConstructType(Set.class,ConstructType.ITERABLE);
    setConstructType(Collection.class,ConstructType.ITERABLE);
    setConstructType(Iterable.class,ConstructType.ITERABLE);
  }

  public <T> void setConstructType(Class<T> type, ConstructType constructType) {
    classToConstructTypes.put(type, constructType);
  }

  public <T> ConstructType getConstructType(Class<T> type) {
    return getConstructType(TypeToken.of(type));
  }

  public <T> ConstructType getConstructType(TypeToken<T> token) {
    if(token.isArray()) {
      return ConstructType.ITERABLE;
    }
    // TODO 父子关系生效?
    Optional<ConstructType> optional = token.getTypes().rawTypes().stream()
        .filter(classToConstructTypes::containsKey).findFirst().map(classToConstructTypes::get);
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
    // TODO 父子关系生效?
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
