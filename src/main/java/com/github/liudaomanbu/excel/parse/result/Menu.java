package com.github.liudaomanbu.excel.parse.result;

import com.github.liudaomanbu.excel.validator.Validators;
import java.lang.reflect.Field;
import java.util.Collection;
import java.util.Objects;
import java.util.Optional;
import java.util.function.Function;
import java.util.stream.Stream;
import com.github.liudaomanbu.excel.config.MenuConfig;
import com.github.liudaomanbu.excel.constant.Direction;
import com.github.liudaomanbu.excel.matcher.data.type.BaseDataType;
import com.github.liudaomanbu.excel.parse.error.ValidationError;
import com.github.liudaomanbu.excel.validator.BaseValidator;
import com.github.liudaomanbu.excel.validator.Validator;
import com.google.common.base.Joiner;
import com.google.common.base.Preconditions;
import com.google.common.base.Predicate;
import com.google.common.collect.ImmutableList;
import com.google.common.collect.ImmutableMap;

public class Menu<T> {
  public static class Builder<T> {
    private StandardCell cell;
    private MenuConfig<T> config;
    private Table<T> table;
    private Menu<T> parent;

    public Menu<T> build() {
      return new Menu<>(this);
    }

    public StandardCell getCell() {
      return cell;
    }

    public Builder<T> setCell(StandardCell cell) {
      this.cell = cell;
      return this;
    }

    public MenuConfig<T> getConfig() {
      return config;
    }

    public Builder<T> setConfig(MenuConfig<T> config) {
      this.config = config;
      return this;
    }

    public Table<T> getTable() {
      return table;
    }

    public Builder<T> setTable(Table<T> table) {
      this.table = table;
      return this;
    }

    public Menu<T> getParent() {
      return parent;
    }

    public Builder<T> setParent(Menu<T> parent) {
      this.parent = parent;
      return this;
    }

  }

  public static <T> Builder<T> builder() {
    return new Builder<>();
  }

  private final StandardCell cell;
  private final MenuConfig<T> config;
  private final ImmutableList<ValidationError<Menu<T>>> errors;
  private final Table<T> table;
  private final Menu<T> parent;
  private final ImmutableList<Menu<T>> childrens;
  private final MenuData<T> data;

  public Menu(Builder<T> builder) {
    cell = builder.cell;
    Preconditions.checkNotNull(cell, "cell can't be null");
    config = builder.config;
    parent = builder.parent;
    table = Optional.ofNullable(builder.table)
        .orElse(Optional.ofNullable(parent).map(Menu::getTable).orElse(null));
    Preconditions.checkNotNull(table, "table can't be null");
    childrens =
        loadChildrens().peek(childrenBuilder -> childrenBuilder.setParent(this).setTable(table))
            .map(Builder::build).collect(ImmutableList.toImmutableList());
    data = new MenuData<>(this);

    errors = getValidators().filter(validator -> validator.needValidate(this))
        .map(validator -> validator.validate(this)).flatMap(Collection::stream)
        .collect(ImmutableList.toImmutableList());
  }

  private Stream<Builder<T>> loadChildrens() {
    ImmutableList<StandardCell> menuCells =
        config.getDirection().get(getCell(), config.getDistance());
    return config.getChildrens().stream()
        .map(t -> menuCells.stream().filter(t::matches).findAny()
            .map(cell -> Menu.<T>builder().setCell(cell).setParent(this).setConfig(t)))
        .filter(Optional::isPresent).map(Optional::get);
  }

  private Stream<Validator<Menu<T>>> getValidators() {
    return Stream.of(createMenuConfigValidator());
  }

  private Validator<Menu<T>> createMenuConfigValidator() {
    return Validators.create(
        config.getChildrens().stream().collect(ImmutableMap.toImmutableMap(children -> {
          Predicate<Menu<T>> predicate = menu -> menu.getChildrens().stream().map(Menu::getConfig)
              .filter(children::equals).findAny().isPresent();
          return predicate;
        }, children -> {
          Function<Menu<T>, String> function = menu -> "没有匹配到" + children.getId() + "对应的菜单";
          return function;
        })));
  }

  public Optional<Menu<T>> getSuper(Predicate<? super Menu<T>> predicate) {
    Preconditions.checkNotNull(predicate);
    if (isTopMenu()) {
      return Optional.empty();
    }
    return predicate.apply(parent) ? Optional.of(parent) : parent.getSuper(predicate);
  }

  public Optional<Menu<T>> getFieldParent() {
    return getSuper(menu -> Objects.nonNull(menu.getFieldName()));
  }

  public ImmutableList<Menu<T>> getSubs(Predicate<? super Menu<T>> predicate) {
    Preconditions.checkNotNull(predicate);
    if (isDataMenu()) {
      return ImmutableList.of();
    }
    ImmutableList<Menu<T>> subs =
        childrens.stream().filter(predicate).collect(ImmutableList.toImmutableList());
    return subs.isEmpty() ? childrens.stream().flatMap(menu -> menu.getSubs(predicate).stream())
        .collect(ImmutableList.toImmutableList()) : subs;
  }

  public ImmutableList<Menu<T>> getFieldChildrens() {
    return getSubs(menu -> Objects.nonNull(menu.getFieldName()));
  }

  public ImmutableList<Field> getFields() {
    if (Objects.isNull(getField())) {
      return ImmutableList.of();
    }
    ImmutableList.Builder<Field> builder = ImmutableList.builder();
    builder.add(getField());
    Optional<Menu<T>> optional = getFieldParent();
    while (optional.isPresent()) {
      Menu<T> menu = optional.get();
      builder.add(menu.getField());
      optional = menu.getFieldParent();
    }
    return builder.build().reverse();
  }

  public Optional<StandardCell> nextDataCell(StandardCell cell) {
    if (Objects.isNull(cell)) {
      cell = this.cell;
    }

    Direction direction = config.getDirection();

    return this.cell.equals(cell) ? direction.getCell(cell, config.getDistance())
        : direction.nextCell(cell);
  }

  public boolean hasChildren(Menu<T> childrenMenu) {
    return childrens.contains(childrenMenu);
  }

  public boolean hasChildren(StandardCell cell) {
    return childrens.stream().anyMatch(childrenMenu -> childrenMenu.getCell().equals(cell));
  }

  public ImmutableList<ValidationError<Menu<T>>> getAllErrors() {
    return Stream
        .concat(errors.stream(),
            childrens.stream().map(Menu::getAllErrors).flatMap(Collection::stream)
                .map(error -> new ValidationError<>(this, error.getMessage())))
        .collect(ImmutableList.toImmutableList());
  }

  public boolean hasError() {
    return !getAllErrors().isEmpty();
  }

  public String getName() {
    return BaseDataType.STRING.cast(cell.getValueCell(), String.class);
  }

  public String getFullName() {
    ImmutableList.Builder<Menu<T>> supers = ImmutableList.builder();
    for (Menu<T> menu = this; Objects.nonNull(menu.parent); menu = menu.parent) {
      supers.add(menu.parent);
    }
    return Joiner.on("-").join(supers.build().reverse().stream().map(Menu::getName)
        .collect(ImmutableList.toImmutableList()));
  }

  public Field getField() {
    return config.getField();
  }

  public String getFieldName() {
    return config.getFieldName();
  }

  // delegate methods start
  public boolean isTopMenu() {
    return config.isTopMenu();
  }

  public boolean isDataMenu() {
    return config.isDataMenu();
  }

  public boolean isSingleDataMenu() {
    return config.isSingleDataMenu();
  }

  // public boolean isFixedDataMenu() {
  // return menuConfig.isFixedDataMenu();
  // }

  public boolean isUnFixedDataMenu() {
    return config.isUnFixedDataMenu();
  }

  public boolean isMustMenu() {
    return config.isMustMenu();
  }

  public boolean isNotMustMenu() {
    return config.isNotMustMenu();
  }

  // delegate methods end

  public StandardCell getCell() {
    return cell;
  }

  public MenuConfig<T> getConfig() {
    return config;
  }

  public Menu<T> getParent() {
    return parent;
  }

  public Table<T> getTable() {
    return table;
  }

  public ImmutableList<Menu<T>> getChildrens() {
    return childrens;
  }

  public MenuData<T> getData() {
    return data;
  }

  public ImmutableList<ValidationError<Menu<T>>> getErrors() {
    return errors;
  }

}
