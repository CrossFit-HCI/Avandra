import React, { ReactElement, ReactNode, createElement, useState } from 'react';
import { Button, View } from 'react-native';

import NavProvider, { NavContext, NavScreenProps, getNavScreen, createNavContext, NaviButton, useAppDispatch, toggleNav, useAppSelector, isNavOpenedSelector, linkScreens, NavBarButton  } from "../nav/model";
import { navBarStyleSheet, navComponentStyleSheet } from '../nav/styles';

const NavView = ({context} : {context: NavContext}) => {  
  const isNavOpened = useAppSelector((state) => isNavOpenedSelector(state));
  const currentScreen = useAppSelector((state) => getNavScreen(context, state));    

  const dispatch = useAppDispatch();
  const togNav = () => dispatch(toggleNav());  

  const toggleButtonTitle: string = isNavOpened ? 'Close' : 'Open';

  return (    
      <View style={navComponentStyleSheet(isNavOpened).container}>          
        <View
          style={navBarStyleSheet.bar}>                    
          <NavBarButton 
            title={toggleButtonTitle}  onPress={() => {togNav()}}/>
        </View>            
        {currentScreen}
      </View>      
  )
}

export const NavScreen = ({label, screen}:NavScreenProps) => { 
  return createElement('NavScreen', {label: label, screen: screen}, <View/>) 
}

interface NavModalsProps {
  children: ReactNode;
}

export interface NavScreensProps {
  children: ReactNode;
  main: ReactElement;
}

export const Nav = ({children, main}:NavScreensProps) => {  
  const dispatch = useAppDispatch();

  // 1. Extract the screen data from each of the children.  
  let newContext = createNavContext(main, children);

  // 2. Link the context with the Redux state.
  newContext.screens.stack.map((screen, index) => {
    dispatch(linkScreens({key: screen.label, value: index}));                    
  });    
  
  const [context] = useState(newContext);

  // 3. Render the main screen:
  return  <NavView context={context}/>;
}

/* We don't want library users to have access to the view model. So we export 
 * the provider here. */
export { NavProvider, NaviButton };
export default Nav;
