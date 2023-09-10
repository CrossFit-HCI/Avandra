import { StyleSheet } from 'react-native';

import { colorDF, borderTheme } from '../../designFramework/DF';

const navHeight: number = 50;

export const navComponentStyleSheet = (isNavOpened: boolean) => {
    // Used to hide and show part of the Nav; this mimics the Nav being open or
    // closed.    
    const navBottom: number = isNavOpened ? 0 : -43;
    
    return StyleSheet.create({
        container: {  
            flex: 1,                      
            height: `${navHeight}%`,
            width: '100%',
            alignContent: 'flex-end',
            backgroundColor: colorDF.component.background,
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
