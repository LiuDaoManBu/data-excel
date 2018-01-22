package com.github.liudaomanbu.excel.parse.result;

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
import com.google.common.collect.ImmutableCollection;
import com.google.common.collect.ImmutableList;
import com.google.common.collect.ImmutableMap;
import com.google.common.collect.ImmutableSet;
import com.google.common.collect.Iterables;

public class Menu {
  public static class Builder {
    private StandardCell cell;
    private MenuConfig config;
    private Table table;
    private Menu parent;

    public Menu build() {
      return new Menu(this);
    }

    public StandardCell getCell() {
      return cell;
    }

    public Builder setCell(StandardCell cell) {
      this.cell = cell;
      return this;
    }

    public MenuConfig getConfig() {
      return config;
    }

    public Builder setConfig(MenuConfig config) {
      this.config = config;
      return this;
    }

    public Table getTable() {
      return table;
    }

    public Builder setTable(Table table) {
      this.table = table;
      return this;
    }

    public Menu getParent() {
      return parent;
    }

    public Builder setParent(Menu parent) {
      this.parent = parent;
      return this;
    }

  }

  public static Builder builder() {
    return new Builder();
  }

  private final StandardCell cell;
  private final MenuConfig config;
  private final ImmutableList<ValidationError<Menu>> errors;
  private final Table table;
  private final Menu parent;
  private final ImmutableList<Menu> childrens;
  private final MenuData data;

  public Menu(Builder builder) {
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
    data = new MenuData(this);

    errors = createMenuConfigValidator().validate(this).stream()
        .collect(ImmutableList.toImmutableList());
  }

  private Stream<Builder> loadChildrens() {
    ImmutableList<StandardCell> menuCells =
        config.getDirection().get(getCell(), config.getDistance());
    return config.getChildrens().stream()
        .map(t -> menuCells.stream().filter(t::matches).findAny()
            .map(cell -> builder().setCell(cell).setParent(this).setConfig(t)))
        .filter(Optional::isPresent).map(Optional::get);
  }

  private Validator<Menu> createMenuConfigValidator() {
    return new BaseValidator<>(
        config.getChildrens().stream().collect(ImmutableMap.toImmutableMap(children -> {
          Predicate<Menu> predicate = menu -> menu.getChildrens().stream().map(Menu::getConfig)
              .filter(children::equals).findAny().isPresent();
          return predicate;
        }, children -> {
          Function<Menu, String> function = menu -> "没有匹配到" + children.getId() + "对应的菜单";
          return function;
        })));
  }

  public Optional<Menu> getSuper(Predicate<? super Menu> predicate) {
    Preconditions.checkNotNull(predicate);
    if (isTopMenu()) {
      return Optional.empty();
    }
    return predicate.apply(parent) ? Optional.of(parent) : parent.getSuper(predicate);
  }

  public Optional<Menu> getFieldParent() {
    return getSuper(menu -> Objects.nonNull(menu.getFieldName()));
  }

  public ImmutableList<Menu> getSubs(Predicate<? super Menu> predicate) {
    Preconditions.checkNotNull(predicate);
    if (isDataMenu()) {
      return ImmutableList.of();
    }
    ImmutableList<Menu> subs =
        childrens.stream().filter(predicate).collect(ImmutableList.toImmutableList());
    return subs.isEmpty() ? childrens.stream().flatMap(menu -> menu.getSubs(predicate).stream())
        .collect(ImmutableList.toImmutableList()) : subs;
  }

  public ImmutableList<Menu> getFieldChildrens() {
    return getSubs(menu -> Objects.nonNull(menu.getFieldName()));
  }

  public ImmutableList<Field> getFields() {
    if (Objects.isNull(getField())) {
      return ImmutableList.of();
    }
    ImmutableList.Builder<Field> builder = ImmutableList.builder();
    builder.add(getField());
    Optional<Menu> optional = getFieldParent();
    while (optional.isPresent()) {
      Menu menu = optional.get();
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

  public boolean hasChildren(Menu childrenMenu) {
    return childrens.contains(childrenMenu);
  }

  public boolean hasChildren(StandardCell cell) {
    return childrens.stream().anyMatch(childrenMenu -> childrenMenu.getCell().equals(cell));
  }

  public ImmutableList<ValidationError<Menu>> getAllErrors() {
    return Stream
        .concat(errors.stream(),
            childrens.stream().map(Menu::getAllErrors).flatMap(Collection::stream)
                .map(error -> new ValidationError<Menu>(this, error.getMessage())))
        .collect(ImmutableList.toImmutableList());
  }

  public String getName() {
    return BaseDataType.STRING.cast(cell.getValueCell(), String.class);
  }

  public String getFullName() {
    ImmutableList.Builder<Menu> supers = ImmutableList.builder();
    for (Menu menu = this; Objects.nonNull(menu.parent); menu = menu.parent) {
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

  public MenuConfig getConfig() {
    return config;
  }

  public Menu getParent() {
    return parent;
  }

  public Table getTable() {
    return table;
  }

  public ImmutableList<Menu> getChildrens() {
    return childrens;
  }

  public MenuData getData() {
    return data;
  }

  public ImmutableList<ValidationError<Menu>> getErrors() {
    return errors;
  }

}
