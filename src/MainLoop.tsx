import React, { ReactNode } from 'react';
import { Text, View} from 'react-native';
import { NativeStackScreenProps, createNativeStackNavigator } from '@react-navigation/native-stack';
import { NavigationContainer } from '@react-navigation/native';

import Nav, { NavProvider, NavScreen, NaviButton } from "./Nav";
import { CloseScreen, OpenScreen } from './NavViewModel';

type AVRootStackParams = {
  Forecast: undefined;
  JournalList: undefined;
}

type ForecastScreenProps = NativeStackScreenProps<AVRootStackParams, 'Forecast'>;

const ModalScreen = () => {
  return (
    <View>
      <Text 
          style={{
            color: 'white',
            fontSize: 20,
          }}>I'm a modal.</Text>

          <CloseScreen title='Close Modal1'/>
    </View>
  )
}

const Forecast = ({ navigation }:ForecastScreenProps) => {
  let navMainScreen = () => {
    return (
      <View>
        <NaviButton
          onPress={() => navigation.navigate('JournalList')}
          title="Journals" />         
      </View>
    )
  }

  return (
      <View
        style={{
          flex: 1,
          flexDirection: 'column',
          justifyContent: 'center',
          alignItems: 'center',
          backgroundColor: 'purple',              
        }
      }> 
        <Text 
          style={{
            color: 'white',
            fontSize: 20,
          }}>Forecast</Text>

        <OpenScreen
          label='Modal1'
          title="Open Modal1" /> 

        <Nav main={navMainScreen()}>
            <NavScreen label='Modal1'  screen={ModalScreen()} />
        </Nav>                
      </View>
  )
}

type JournalListScreenProps = NativeStackScreenProps<AVRootStackParams, 'JournalList'>;

const JournalList = ({ navigation }:JournalListScreenProps) => {
  let navMainScreen = () => {
    return (
      <View>
        <Text>Journal List Nav</Text>

        <NaviButton
          onPress={() => navigation.navigate('Forecast')}
          title="Forecast" />
      </View>
    )
  }

  return (    
      <View
        style={{
          flex: 1,
          justifyContent: 'center',
          alignItems: 'center',
      }}>
        <Text>Journal List</Text>

        <Nav main={navMainScreen()}>          
        </Nav>
      </View>
  )
};

export interface MainProps {
  children: ReactNode;
}

const AVScreenStack = createNativeStackNavigator<AVRootStackParams>();

const MainLoop = () => {    
  return (
    <NavProvider>
      <NavigationContainer>
        <AVScreenStack.Navigator screenOptions={{headerShown: false }}>
            <AVScreenStack.Screen name="Forecast" component={Forecast} />          
            <AVScreenStack.Screen name="JournalList" component={JournalList}/>
        </AVScreenStack.Navigator>
      </NavigationContainer>
    </NavProvider>
  )
};

export default MainLoop;