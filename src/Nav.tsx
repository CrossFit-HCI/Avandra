import React, { ReactNode, Children, useContext } from 'react';
import {Button, View} from 'react-native';
import { NavContext, NavScreenProps, getNavScreen, getScreens, isNavOpened, openTheNav, toggleNavStatus } from "./NavViewModel";
import { navContainerBarViewStyle, navContainerViewStyle } from './NavStyles';
import { Provider } from 'react-redux';

const NavView = () => {  
  const currentScreen = getNavScreen();

  // Used to hide and show part of the Nav; this mimics the Nav being open or
  // closed.
  let navHeight = '50%';
  let navBottom = isNavOpened() ? '0%' : '-42%';  

  return (    
      <View style={navContainerViewStyle(navHeight, navBottom).container}>          
        <View
          style={navContainerBarViewStyle.container}>                    
          <Button 
            title="Toggle Nav"  onPress={toggleNavStatus}/>
        </View>        
        {currentScreen}
      </View>      
  )
}

export const NavScreen = ({id, screen}:NavScreenProps) => null;

export interface NavScreensProps {
  children: ReactNode;
}

export const NavScreens = ({children}:NavScreensProps) => {  
  // 1. Extract the screen data from each of the children.  
  const screens: NavContext = getScreens(children);

  // 2. Inject the screens into the Nav:
  //injectTheScreens(screens.length);
  // 3. Make sure the Nav is open:
  openTheNav();

  // 4. Render the main screen:
  return (
      <NavView />      
  );
}

export default NavScreens;
