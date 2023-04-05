import React, { ReactNode } from 'react';
import {Button, Text, View} from 'react-native';
import { closeNav, openNav, useAppDispatch, useAppSelector } from "./NavState";
import navStore, { RootState } from './Store';
import { Provider } from 'react-redux';

export interface NavProps {
  children: ReactNode;
}

const Nav = ({children}:NavProps) => {
  const navStatus = useAppSelector((state:RootState) => state.nav.navStatus);
  const dispatch = useAppDispatch();

  let navHeight = '50%';
  let navBottom = navStatus.status == 'opened' ? '0%' : '-42%';
  let navMsg    = navStatus.status;

  return (    
      <View 
        style={{
          height: navHeight,
          width: '100%',
          justifyContent: 'center',      
          alignItems: 'center',
          backgroundColor: 'grey',
          position: 'absolute',
          bottom: navBottom,        
        }
      }>
        <Text
          style={{
            fontSize: 20,
            position: 'absolute',
            top: 10,
          }}>
            The Nav is {navMsg}
        </Text>            
        <Button title="Open Nav"  onPress={() => dispatch(openNav())}/>
        <Button title="Close Nav" onPress={() => dispatch(closeNav())}/>
        {children}
      </View>
  )
}

export default ({children}:NavProps) => {
  return (
    <Provider store={navStore}>
      <Nav>
        {children}
      </Nav>
    </Provider>
  )
}
