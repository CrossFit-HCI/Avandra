import React, { ReactElement, ReactNode, createElement, useState } from 'react';
import { Button, View } from 'react-native';

import NavProvider, { NavContext, NavScreenProps, getNavScreen, createNavContext, NaviButton, useAppDispatch, toggleNav, useAppSelector, isNavOpenedSelector, linkStackScreens, linkModalScreens  } from "./NavViewModel";
import { navContainerBarViewStyle, navContainerViewStyle } from './NavStyles';

const NavView = ({context} : {context: NavContext}) => {  
  const isNavOpened = useAppSelector((state) => isNavOpenedSelector(state));
  const currentScreen = useAppSelector((state) => getNavScreen(context, state));    

  const dispatch = useAppDispatch();
  const togNav = () => dispatch(toggleNav());  

  return (    
      <View style={navContainerViewStyle(isNavOpened).container}>          
        <View
          style={navContainerBarViewStyle.container}>                    
          <Button 
            title="Toggle Nav"  onPress={() => {togNav()}}/>
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

export const NavModals = ({children}:NavModalsProps): JSX.Element => {
  return createElement('NavModals', {children: children}, <View/>) 
}

export interface NavScreensProps {
  children: ReactNode;
  main: ReactElement;
}

export const Nav = ({children, main}:NavScreensProps) => {  
  const dispatch = useAppDispatch();

  // 1. Extract the screen data from each of the children.  
  let newContext = createNavContext(main, children);

  // 2. Link the context with the state:
  newContext.stacks.map((stack, index) => {
    dispatch(linkStackScreens({key: stack.label, value: index}));                    
  });

  newContext.modals.map((modal, index) => {
    dispatch(linkModalScreens({key: modal.label, value: index}));                    
  });    
  
  const [context] = useState(newContext);

  // 3. Render the main screen:
  return  <NavView context={context}/>;
}

/* We don't want library users to have access to the view model. So we export 
 * the provider here. */
export { NavProvider, NaviButton };
export default Nav;
