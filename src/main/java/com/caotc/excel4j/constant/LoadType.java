package com.caotc.excel4j.constant;

import java.util.Collection;

import org.apache.poi.ss.usermodel.Cell;

import com.caotc.excel4j.config.MenuConfig;
import com.caotc.excel4j.parse.result.Menu;

public enum LoadType {
	DYNAMIC {
		@Override
		public void loadChildren(Menu menu) {
			
		}
	},FIXED {
		@Override
		public void loadChildren(Menu menu) {
			MenuConfig config=menu.getCheckMenuConfig();
			Collection<MenuConfig> childrenConfigs=config.getChildrenMenuConfigs();
			Cell firstCell=config.getDirection().getCell(menu.getCell(), config.getFirstDistance());
		}
	};
	public abstract void loadChildren(Menu menu);
}
