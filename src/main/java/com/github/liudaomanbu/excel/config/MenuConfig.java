package com.github.liudaomanbu.excel.config;

import java.lang.reflect.Field;
import java.util.Collection;
import java.util.List;
import java.util.Objects;
import java.util.Optional;
import java.util.function.Predicate;
import com.github.liudaomanbu.excel.constant.Direction;
import com.github.liudaomanbu.excel.constant.LoadType;
import com.github.liudaomanbu.excel.constant.Necessity;
import com.github.liudaomanbu.excel.matcher.Matcher;
import com.github.liudaomanbu.excel.parse.result.Menu;
import com.github.liudaomanbu.excel.parse.result.StandardCell;
import com.github.liudaomanbu.excel.validator.Validator;
import com.google.common.base.Preconditions;
import com.google.common.collect.ImmutableCollection;
import com.google.common.collect.ImmutableList;
import com.google.common.collect.ImmutableSet;
import com.google.common.collect.Lists;

public class MenuConfig<T> extends Config {
  public static class Builder<T> extends Config.Builder {
    private TableConfig<T> tableConfig;
    private MenuConfig<T> parent;
    private MenuDataConfig.Builder<T> dataConfigBuilder;
    private Collection<MenuConfig.Builder<T>> childrenBuilders;
    // 菜单匹配器
    private Matcher<StandardCell> matcher;
    private List<Validator<Menu<T>>> validators;
    // 第一个数据单元格相对于菜单单元格的单元格距离
    private Integer distance;
    private Necessity necessity;
    private Direction direction;
    private ParserConfig parserConfig;

    public Builder() {
      childrenBuilders = Lists.newLinkedList();
      validators = Lists.newLinkedList();
    }

    public MenuConfig<T> build() {
      return new MenuConfig<>(this);
    }

    @Override
    public Builder<T> setId(Object id) {
      super.setId(id);
      return this;
    }

    public TableConfig<T> getTableConfig() {
      return tableConfig;
    }

    public List<Validator<Menu<T>>> getValidators() {
      return validators;
    }

    public Builder<T> setValidators(List<Validator<Menu<T>>> validators) {
      this.validators = validators;
      return this;
    }

    public Builder<T> setTableConfig(TableConfig<T> tableConfig) {
      this.tableConfig = tableConfig;
      return this;
    }

    public Integer getDistance() {
      return distance;
    }

    public Necessity getNecessity() {
      return necessity;
    }

    public Builder<T> setNecessity(Necessity necessity) {
      this.necessity = necessity;
      return this;
    }

    public Direction getDirection() {
      return direction;
    }

    public Builder<T> setDirection(Direction direction) {
      this.direction = direction;
      return this;
    }

    public MenuConfig<T> getParent() {
      return parent;
    }

    public Builder<T> setParent(MenuConfig<T> parent) {
      this.parent = parent;
      return this;
    }

    public MenuDataConfig.Builder<T> getDataConfigBuilder() {
      return dataConfigBuilder;
    }

    public Builder<T> setDataConfigBuilder(MenuDataConfig.Builder<T> dataConfigBuilder) {
      this.dataConfigBuilder = dataConfigBuilder;
      return this;
    }

    public Collection<MenuConfig.Builder<T>> getChildrenBuilders() {
      return childrenBuilders;
    }

    public Builder<T> setChildrenBuilders(Collection<MenuConfig.Builder<T>> childrenBuilders) {
      this.childrenBuilders = childrenBuilders;
      return this;
    }

    public Matcher<StandardCell> getMatcher() {
      return matcher;
    }

    public Builder<T> setMatcher(Matcher<StandardCell> matcher) {
      this.matcher = matcher;
      return this;
    }

    public Builder<T> setDistance(Integer distance) {
      this.distance = distance;
      return this;
    }

    public ParserConfig getParserConfig() {
      return parserConfig;
    }

    public Builder<T> setParserConfig(ParserConfig parserConfig) {
      this.parserConfig = parserConfig;
      return this;
    }

  }

  public static final int DEFAULT_DISTANCE = 1;
  private static final Necessity DEFAULT_MENU_NECESSITY = Necessity.MUST;

  public static <T> Builder<T> builder() {
    return new Builder<>();
  }

  private final TableConfig<T> tableConfig;
  // 菜单匹配器
  private final Predicate<StandardCell> matcher;
  // 第一个数据单元格相对于菜单单元格的单元格距离
  private final int distance;
  private final Necessity necessity;
  private final Direction direction;
  private final MenuConfig<T> parent;
  private final ImmutableCollection<MenuConfig<T>> childrens;
  private final ImmutableList<Validator<Menu<T>>> validators;
  private final MenuDataConfig<T> dataConfig;
  private final ParserConfig parserConfig;

  private MenuConfig(Builder<T> builder) {
    super(builder);
    Preconditions.checkNotNull(builder.matcher, "matcher can't be null");
    matcher = builder.matcher.reduce();


    distance = Optional.ofNullable(builder.distance).orElse(DEFAULT_DISTANCE);
    parent = builder.parent;
    necessity = Optional.ofNullable(builder.necessity).orElse(DEFAULT_MENU_NECESSITY);
    tableConfig = Optional.ofNullable(builder.tableConfig)
        .orElse(Optional.ofNullable(parent).map(MenuConfig::getTableConfig).orElse(null));
    Preconditions.checkNotNull(tableConfig, "tableConfig can't be null");
    direction = Optional.ofNullable(builder.direction).orElse(Optional.ofNullable(parent)
        .map(MenuConfig::getDirection).orElse(tableConfig.getMenuDirection()));
    childrens = Optional.ofNullable(builder.childrenBuilders).orElse(ImmutableList.of()).stream()
        .peek(childrenBuilder -> childrenBuilder.setParent(this).setTableConfig(tableConfig))
        .map(Builder::build).collect(ImmutableSet.toImmutableSet());
    validators = builder.validators.stream().collect(ImmutableList.toImmutableList());
    dataConfig = builder.dataConfigBuilder.setMenuConfig(this).build();
    parserConfig = builder.parserConfig;

    Preconditions.checkState(!(!childrens.isEmpty() && Objects.nonNull(dataConfig)));
  }

  public Field getField() {
    return Optional.ofNullable(dataConfig).map(MenuDataConfig::getField).orElse(null);
  }

  public String getFieldName() {
    return Optional.ofNullable(dataConfig).map(MenuDataConfig::getFieldName).orElse(null);
  }

  public boolean isTopMenu() {
    return Objects.isNull(parent);
  }

  public boolean isMustMenu() {
    return Necessity.MUST.equals(necessity);
  }

  public boolean isNotMustMenu() {
    return Necessity.NOT_MUST.equals(necessity);
  }

  public boolean isDataMenu() {
    return childrens.isEmpty();
  }

  public boolean isSingleDataMenu() {
    return isDataMenu() && LoadType.SINGLE.equals(dataConfig.getLoadType());
  }

  // public boolean isFixedDataMenu() {
  // return isDataMenu() && LoadType.FIXED.equals(dataConfig.getLoadType());
  // }

  public boolean isUnFixedDataMenu() {
    return isDataMenu() && LoadType.UNFIXED.equals(dataConfig.getLoadType());
  }

  public ParserConfig getEffectiveParserConfig() {
    return Optional.ofNullable(parserConfig).orElse(Optional.ofNullable(parent)
        .map(MenuConfig::getParserConfig).orElse(tableConfig.getEffectiveParserConfig()));
  }

  // delegate methods start

  public boolean matches(StandardCell cell) {
    return matcher.test(cell);
  }

  // delegate methods end

  public Predicate<StandardCell> getMatcher() {
    return matcher;
  }

  public int getDistance() {
    return distance;
  }

  public MenuConfig<T> getParent() {
    return parent;
  }

  public Direction getDirection() {
    return direction;
  }

  public Necessity getNecessity() {
    return necessity;
  }

  public MenuDataConfig<T> getDataConfig() {
    return dataConfig;
  }

  public ImmutableCollection<MenuConfig<T>> getChildrens() {
    return childrens;
  }

  public TableConfig<T> getTableConfig() {
    return tableConfig;
  }

  public ParserConfig getParserConfig() {
    return parserConfig;
  }

  public ImmutableList<Validator<Menu<T>>> getValidators() {
    return validators;
  }

}
