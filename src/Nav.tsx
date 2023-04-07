import React, { ReactNode } from 'react';
import {Button, View} from 'react-native';
import { toggleNav, useAppDispatch, useAppSelector } from "./NavState";
import { navContainerBarViewStyle, navContainerViewStyle } from './NavStyles';
import navStore, { RootState } from './NavStore';
import { Provider } from 'react-redux';

interface NavViewProps {
  children: ReactNode
}

const NavView = ({ children }:NavViewProps) => {
  const navStatus = useAppSelector((state:RootState) => state.nav.navStatus);
  const dispatch = useAppDispatch();    
  const toggleNavStatus = () => dispatch(toggleNav())

  let navHeight = '50%';
  let navBottom = navStatus.status == 'opened' ? '0%' : '-42%';  

  return (    
      <View style={navContainerViewStyle(navHeight, navBottom).container}>
          
        <View
          style={navContainerBarViewStyle.container}>                    
          <Button 
            title="Toggle Nav"  onPress={toggleNavStatus}/>
        </View>        
        {children}
      </View>      
  )
}

export interface NavProps {
  children: ReactNode;
}

export default ({children}:NavProps) => {
  return (
    <Provider store={navStore}>
      <NavView>
          {children}
      </NavView>
    </Provider>
  )
}
