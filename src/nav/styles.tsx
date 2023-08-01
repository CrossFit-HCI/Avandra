import { StyleSheet } from "react-native";

import { colorTheme, borderTheme } from "../theme/theme";

export const navComponentStyleSheet = (isNavOpened: boolean) => {
    // Used to hide and show part of the Nav; this mimics the Nav being open or
    // closed.
    let navHeight: number = 50;
    let navBottom: number = isNavOpened ? 0 : -42;
    
    return StyleSheet.create({
        container: {
            height: `${navHeight}%`,
            width: '100%',
            justifyContent: 'center',      
            alignItems: 'center',
            backgroundColor: colorTheme.component.background,
            position: 'absolute',
            bottom: `${navBottom}%`,
            borderTopColor: borderTheme.component.borderColor,
            borderTopWidth: borderTheme.component.borderWidth         
        }        
    });
};

export const navComponentButtonStyleSheet = StyleSheet.create({
    primaryContainer: {
        height: 83,
        width: 83,
        backgroundColor: 'black',
        justifyContent: 'center',
        alignItems: 'center',
        elevation: 1
    },
    primaryText: {
        color: 'white',
        fontSize: 15,        
    }
});

export const navBarStyleSheet = StyleSheet.create({
    bar: {
        flex: 1,
        width: '100%',
        justifyContent: 'center',      
        alignItems: 'flex-end',
        position: 'absolute',
        top: 2.5,
        padding: 8
    }
});

export const navBarButtonStyleSheet = StyleSheet.create({
    primaryContainer: {
        height: 30,
        width: 78,
        backgroundColor: 'black',
        justifyContent: 'center',
        alignItems: 'center',
        elevation: 1
    },
    primaryText: {
        color: 'white',
        fontSize: 18,        
    }
});