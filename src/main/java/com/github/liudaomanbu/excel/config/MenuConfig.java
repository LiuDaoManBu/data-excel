package com.github.liudaomanbu.excel.config;

import java.lang.reflect.Field;
import java.util.List;
import java.util.Objects;
import java.util.Optional;
import java.util.function.Predicate;
import com.github.liudaomanbu.excel.config.Config.Builder;
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

public class MenuConfig extends Config {
  public static class Builder extends Config.Builder {
    private TableConfig tableConfig;
    private MenuConfig parent;
    private MenuDataConfig.Builder dataConfigBuilder;
    private List<MenuConfig.Builder> childrenBuilders;
    // 菜单匹配器
    private Matcher<StandardCell> matcher;
    private List<Validator<Menu>> validators;
    // 第一个数据单元格相对于菜单单元格的单元格距离
    private Integer distance;
    private Necessity necessity;
    private Direction direction;
    private ParserConfig parserConfig;

    public Builder() {
      childrenBuilders = Lists.newLinkedList();
      validators = Lists.newLinkedList();
    }

    public MenuConfig build() {
      return new MenuConfig(this);
    }

    @Override
    public Builder setId(Object id) {
      super.setId(id);
      return this;
    }

    public TableConfig getTableConfig() {
      return tableConfig;
    }

    public List<Validator<Menu>> getValidators() {
      return validators;
    }

    public Builder setValidators(List<Validator<Menu>> validators) {
      this.validators = validators;
      return this;
    }

    public Builder setTableConfig(TableConfig tableConfig) {
      this.tableConfig = tableConfig;
      return this;
    }

    public Integer getDistance() {
      return distance;
    }

    public Necessity getNecessity() {
      return necessity;
    }

    public Builder setNecessity(Necessity necessity) {
      this.necessity = necessity;
      return this;
    }

    public Direction getDirection() {
      return direction;
    }

    public Builder setDirection(Direction direction) {
      this.direction = direction;
      return this;
    }

    public MenuConfig getParent() {
      return parent;
    }

    public Builder setParent(MenuConfig parent) {
      this.parent = parent;
      return this;
    }

    public MenuDataConfig.Builder getDataConfigBuilder() {
      return dataConfigBuilder;
    }

    public Builder setDataConfigBuilder(MenuDataConfig.Builder dataConfigBuilder) {
      this.dataConfigBuilder = dataConfigBuilder;
      return this;
    }

    public List<MenuConfig.Builder> getChildrenBuilders() {
      return childrenBuilders;
    }

    public Builder setChildrenBuilders(List<MenuConfig.Builder> childrenBuilders) {
      this.childrenBuilders = childrenBuilders;
      return this;
    }

    public Matcher<StandardCell> getMatcher() {
      return matcher;
    }

    public Builder setMatcher(Matcher<StandardCell> matcher) {
      this.matcher = matcher;
      return this;
    }

    public Builder setDistance(Integer distance) {
      this.distance = distance;
      return this;
    }

    public ParserConfig getParserConfig() {
      return parserConfig;
    }

    public Builder setParserConfig(ParserConfig parserConfig) {
      this.parserConfig = parserConfig;
      return this;
    }

  }

  public static final int DEFAULT_DISTANCE = 1;
  private static final Necessity DEFAULT_MENU_NECESSITY = Necessity.MUST;

  public static Builder builder() {
    return new Builder();
  }

  private final TableConfig tableConfig;
  // 菜单匹配器
  private final Predicate<StandardCell> matcher;
  // 第一个数据单元格相对于菜单单元格的单元格距离
  private final int distance;
  private final Necessity necessity;
  private final Direction direction;
  private final MenuConfig parent;
  private final ImmutableCollection<MenuConfig> childrens;
  private final ImmutableList<Validator<Menu>> validators;
  private final MenuDataConfig dataConfig;
  private final ParserConfig parserConfig;

  private MenuConfig(Builder builder) {
    super(builder);
    // TODO tip
    Preconditions.checkNotNull(builder.matcher);
    matcher = builder.matcher.reduce();


    distance = Optional.ofNullable(builder.distance).orElse(DEFAULT_DISTANCE);
    parent = builder.parent;
    necessity = Optional.ofNullable(builder.necessity).orElse(DEFAULT_MENU_NECESSITY);
    tableConfig = Optional.ofNullable(builder.tableConfig)
        .orElse(Optional.ofNullable(parent).map(MenuConfig::getTableConfig).orElse(null));
    // TODO tip
    Preconditions.checkState(Objects.nonNull(tableConfig));
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

  public MenuConfig getParent() {
    return parent;
  }

  public Direction getDirection() {
    return direction;
  }

  public Necessity getNecessity() {
    return necessity;
  }

  public MenuDataConfig getDataConfig() {
    return dataConfig;
  }

  public ImmutableCollection<MenuConfig> getChildrens() {
    return childrens;
  }

  public TableConfig getTableConfig() {
    return tableConfig;
  }

  public ParserConfig getParserConfig() {
    return parserConfig;
  }

  public ImmutableList<Validator<Menu>> getValidators() {
    return validators;
  }

}
