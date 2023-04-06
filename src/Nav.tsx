import React, { ReactNode, useState } from 'react';
import {Button, Text, View} from 'react-native';
import { navClosed, navOpened } from "./NavState";

export interface NavProps {
  children: ReactNode;
}

const Nav = ({children}:NavProps) => {
  const [navStatus, setaNavStatus] = useState(navOpened);  

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
          backgroundColor: 'lightgrey',
          position: 'absolute',
          bottom: navBottom,        
        }
      }>
        <View
          style={{
            height: 30,
            width: '100%',
            justifyContent: 'center',      
            alignItems: 'center',
            backgroundColor: 'grey',
            position: 'absolute',
            top: 0
          }
        }>
          <Text
            style={{
              fontSize: 20,
              position: 'absolute',
              top: 0,
            }}>
              The Nav is {navMsg}
          </Text>            
          <Button 
            title="Toggle Nav"  onPress={() => navStatus.status == 'opened' ? setaNavStatus(navOpened) : setaNavStatus(navClosed)}/>
        </View>        
        {children}
      </View>      
  )
}

export default ({children}:NavProps) => {
  return (
    <Nav>
        {children}
    </Nav>
  )
}
