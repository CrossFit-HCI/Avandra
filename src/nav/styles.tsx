import { StyleSheet } from "react-native";

import { colorTheme, borderTheme } from "../theme/theme";

let navHeight: number = 50;

export const navComponentStyleSheet = (isNavOpened: boolean) => {
    // Used to hide and show part of the Nav; this mimics the Nav being open or
    // closed.    
    let navBottom: number = isNavOpened ? 0 : -43;
    
    return StyleSheet.create({
        container: {  
            flex: 1,                      
            height: `${navHeight}%`,
            width: '100%',
            alignContent: 'flex-end',
            backgroundColor: colorTheme.component.background,
            position: 'absolute',
            bottom: `${navBottom}%`,
            borderTopColor: borderTheme.component.borderColor,
            borderTopWidth: borderTheme.component.borderWidth         
        }        
    });
};

export const navCustomViewStyleSheet = StyleSheet.create({
    container: {
        flex: 7,
        padding: 3,
        width: '100%',       
    }
});

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
        fontSize: 18,        
    }
});

export const navBarStyleSheet = StyleSheet.create({
    bar: {
        flex: 1,
        width: '100%',
        flexDirection: 'row',
        alignItems: 'flex-end',  
        gap: 8,      
        paddingHorizontal: 8,
        paddingBottom: 12,
        borderBottomColor: borderTheme.component.borderColor,
        borderBottomWidth: borderTheme.component.borderWidth        
    },
    search: {
        flex: 3,
        height: 30,
        padding: 5,
        borderWidth: borderTheme.component.borderWidth        
    }
});

export const navBarButtonStyleSheet = StyleSheet.create({
    primaryContainer: {
        flex: 1,
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