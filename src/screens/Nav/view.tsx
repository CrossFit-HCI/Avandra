import React, { ReactElement, ReactNode, createElement, useState } from 'react';
import { Button, GestureResponderEvent, TextInput, View } from 'react-native';

import NavProvider, { NavContext, NavScreenProps, getNavScreen, createNavContext, useAppDispatch, toggleNav, useAppSelector, isNavOpenedSelector, linkScreens, ejectScreen, injectScreen, closeNav  } from './model';
import { navBarStyleSheet, navComponentStyleSheet, navCustomViewStyleSheet } from './styles';
import { ControlButton, ControlButtonLarge, ControlButtonProps } from '../../theme/Button/view';

/**
 * A button for navigating to a new screen.
 * @param props - A `NavButtonProps`.
 * @returns A `ControlButton` with the onPress callback wrapped to manage the state of
 * the Nav properly.
 */
const ControlButtonNav = (props: ControlButtonProps) => {
  const dispatch = useAppDispatch();
  const closeTheNav = () => dispatch(closeNav());
  
  const onPressCallback = (event: GestureResponderEvent) => {
      // Make sure the Nav is closed before transitioning to a new screen.
      closeTheNav();
      props.onPress(event);
  };

  return (
      <ControlButtonLarge onPress={onPressCallback} title={props.title} />
  );
};

interface OpenModalProps {
  title: string,
  label: string
}

export const OpenNavModal = (props: OpenModalProps) => {
  const dispatch = useAppDispatch();

  // eslint-disable-next-line @typescript-eslint/no-unused-vars
  const onPressCallback = (event: GestureResponderEvent) => {
      // Switch the current Nav screen to the modal with label.
      dispatch(injectScreen(props.label));
  };

  return (
      <Button title={props.title} onPress={onPressCallback} />
  );
};

export const CloseNavModal = (props: {title: string}) => {
  const dispatch = useAppDispatch();

  // eslint-disable-next-line @typescript-eslint/no-unused-vars
  const onPressCallback = (event: GestureResponderEvent) => {
      // Switch the current Nav screen to the modal with label.
      dispatch(ejectScreen());
  };

  return (
      <Button title={props.title} onPress={onPressCallback} />
  );
};


const NavView = ({context}: {context: NavContext}) => {  
  const isNavOpened = useAppSelector((state) => isNavOpenedSelector(state));
  const currentScreen = useAppSelector((state) => getNavScreen(context, state));    

  const dispatch = useAppDispatch();
  const togNav = () => dispatch(toggleNav());  

  const toggleButtonTitle: string = isNavOpened ? 'Close' : 'Open';
  const toggleInputLabel: string = isNavOpened ? 'Search' : 'Feedback';

  return (
      <View style={navComponentStyleSheet(isNavOpened).container}>          
        <View style={navBarStyleSheet.bar}>
          <TextInput style={navBarStyleSheet.search} value={toggleInputLabel} />
          <ControlButton title={toggleButtonTitle}  onPress={() => {togNav();}} />
        </View>  
        <View style={navCustomViewStyleSheet.container}>
          {currentScreen}
        </View>
      </View>      
  );
};

export const NavScreen = ({label, screen}: NavScreenProps) => { 
  return createElement('NavScreen', {label: label, screen: screen}, <View/>); 
};

export interface NavScreensProps {
  children: ReactNode;
  main: ReactElement;
}

export const Nav = ({children, main}: NavScreensProps) => {  
  const dispatch = useAppDispatch();

  // 1. Extract the screen data from each of the children.  
  const newContext = createNavContext(main, children);

  // 2. Link the context with the Redux state.
  newContext.screens.stack.map((screen, index) => {
    dispatch(linkScreens({key: screen.label, value: index}));                    
  });    
  
  const [context] = useState(newContext);

  // 3. Render the main screen:
  return  <NavView context={context}/>;
};

/* We don't want library users to have access to the view model. So we export 
 * the provider here. */
export { NavProvider, ControlButtonNav };
export default Nav;
